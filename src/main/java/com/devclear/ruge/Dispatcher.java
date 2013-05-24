/*******************************************************************************
 * Copyright 2013 DevClear
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package com.devclear.ruge;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class Dispatcher 
{
	private int bound = 0;
	private int count = 0;
	private final Map<String,Dispatchable> map = new HashMap<String, Dispatchable>();
	private final CmdStats cmdCounter = new CmdStats();
	
	float getCompletionPercentage()
	{
		return count==0 ? 0 : ((count) / (float)bound); 
	}
	
	/**
	 * Registers a worker.
	 * 
	 * @param key by which worker is invoked. May be null, in which case worker is never invoked and is only tracked.
	 * @param worker
	 */
	public void register(String key, Dispatchable worker)
	{
		if (key != null) {
			map.put(key,worker);	
		}
	}
	
	public void ignore(String key)
	{
		register(key,new Dispatchable.Ignore(key));
	}
	
	private LineNumberReader setFileSource(String fileName, int maxInputs) throws IOException
	{
		File file = new File(fileName);
		LineNumberReader br = new LineNumberReader(new FileReader(file));
		if (maxInputs >= 0) {
			bound = maxInputs;
		}
		else {
			bound = 0;
			while (br.readLine() != null) {
				bound++;
			}
			br.close();
		}
		br = new LineNumberReader(new FileReader(file));
		return br;
	}
	
	public void exec(int maxInputs, String comment, Tracker tracker, String inputEventsFileName, int intervalSeconds) throws Exception
	{
		LineNumberReader br = setFileSource(inputEventsFileName,maxInputs);
		long start = System.currentTimeMillis();
		tracker.start(intervalSeconds);
		
		System.out.println("Starting: dispatch " + bound + " times for run \"" + comment + "\"");
		
		try {
			Splitter splitter = new Splitter();
			while (splitter.parse(br) >= 0) {
				int lineNumber = br.getLineNumber();
				if (lineNumber >= bound) {
					break;
				}
				String cmd = splitter.get(0);
				Dispatchable worker = map.get(cmd);
				cmdCounter.bump(worker,cmd);
				
				if (worker == null) {
        			System.err.println("Unknown line in input, command \"" + cmd + "\" in line: " + splitter.getOrg());
				}
				else {
					splitter = worker.prepare(splitter,br);
					count = br.getLineNumber(); 
					worker.handle(splitter);
				}
			}
        } 
        finally {
        	tracker.stop();
        	for (Dispatchable d: map.values()) {
        		d.closeResources();
        	}
        	br.close();
        	printTimeSummary(start,count,comment,cmdCounter.getCounters());
        }
	}
	
	private void printTimeSummary(long start, int count, String comment, Collection<CmdStats.Counter>counters) 
	{
		long end = System.currentTimeMillis();
		long diff = end-start;
		long diffSeconds = diff/ 1000;
		long diffMinutes = diffSeconds / 60;
		long diffMillis = diff % 1000;
		
		diffSeconds = diffSeconds % 60;
		System.out.print("Completed: processed " + count + " inputs in " + diff + "u = (" + diffMinutes + "m, " + diffSeconds + '.' + diffMillis + "s) for run \"" + comment + "\"");
		System.out.println(". Breakdown of commands was as follows:");
		for (CmdStats.Counter counter: counters) {
			CmdStats.Counter actual = map.get(counter.cmd).getStats();
			long av = actual.getExecCount();
			long tv = counter.getExecCount();
			String avs = String.valueOf(av);
			String cc = (av==tv?avs:(avs+'/'+tv)); // actual / target only if these values differ
			System.out.format("%20s ... %s",cc,counter.cmd);
			long errs = actual.getErrCount();
			if (errs > 0) {
				System.out.print(" *** NOTE " + errs + " exceptions of type [");
				boolean first = true;
				for (String s: actual.getErrTypes()) {
					if (!first) System.out.print(',');
					System.out.print(s);
				}
				System.out.print("] !!!");
			}
			System.out.println("");
		}
	}
}



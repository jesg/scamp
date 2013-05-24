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
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

public class Tracker 
{
	private static String LOG_FILE = "data.csv";

	private ScheduledExecutorService executor = null;
	private ScheduledFuture<?> future = null;
	
	public void stop() throws Exception
	{
		if (future != null) future.cancel(true);
		if (executor != null) executor.shutdownNow();
		for (Trackable t: trackables) {
			t.stop();
		}
	}
	
	private long started = 0;
	private long count = 0;
	private long intervalSeconds = 0;
	private final String comment;
	private final List<Trackable> trackables = new ArrayList<Trackable>();
	public Tracker(String comment)
	{
		this.comment = comment;
	}
	
	public void track(Trackable t)
	{
		if (!trackables.contains(t)) {
			trackables.add(t);
		}
	}
	
	private synchronized void writeTrackingEntry(FileWriter fw) throws IOException 
	{
		fw.write(comment);
		writeEntry(fw,Long.toString(started));
		writeEntry(fw,Long.toString(count * intervalSeconds));
		for (Trackable t: trackables) {
			t.reportData(fw);
		}
		fw.write('\n');
		fw.flush();
	}
	
	public static void writeEntry(FileWriter fw, String v) throws IOException
	{
		fw.write(',');
		fw.write(v);
	}

	public void start(int intervalSeconds) throws Exception
	{
		if (future != null) return;
		
		this.intervalSeconds = intervalSeconds;
		this.count = 0;
		this.started = System.currentTimeMillis();
		
		File file = new File(LOG_FILE);
		boolean missing = !file.exists();
		System.out.print("Setup: " + (missing ? "creating" : "appending to") + " tracking file \"" + LOG_FILE + '"');
		System.out.println(" with entry every " + intervalSeconds + " second" + (intervalSeconds==1?"":"s"));
		
        final FileWriter fw = new FileWriter(file,true);
		if (missing || file.length() == 0) {
			fw.write("Run,Start,Time");
			for (Trackable t: trackables) {
				t.reportHeader(fw);
			}
			fw.write('\n');
			fw.flush();
		}

		Runnable runnable = new Runnable() {
			@Override public void run() {
				try {
					count++;
					writeTrackingEntry(fw);
				} 
				catch (IOException e) {
					System.err.println("Tracker error: " + e.getMessage());
					e.printStackTrace();
				}
			}
		};
		
		executor = Executors.newScheduledThreadPool(1);
		future = executor.scheduleAtFixedRate(runnable,intervalSeconds,intervalSeconds,TimeUnit.SECONDS);
		
		for (Trackable t: trackables) {
			t.start();
		}
	}
}

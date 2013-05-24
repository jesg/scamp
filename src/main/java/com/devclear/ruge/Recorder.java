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

import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.IOException;

public class Recorder implements Dispatchable, Trackable 
{
	private final Tracker tracker;
	
	private final String id;

	private long timeOfFirstCall = 0;
	//private long timeOfLastReport = 0;

	private long numberProcessedSinceLastReport = 0;
	private long totalCallTimeSinceLastReport = 0;
	private long timeOfLastReport = 0;

	private long numberProcessedOverall = 0;
	//private long totalCallTimeOverall = 0;

	private final long whenToStartTrackingTotalTps;
	private long numberProcessedSinceTrackingStart = 0;
	private long totalCallTimeSinceTrackingStart = 0;
	
	public static class Direct extends Recorder
	{
		private Dispatchable target;
	
		public Direct(Tracker tracker, String id, long whenToStartTrackingTotalTps, Dispatchable target)
		{
			super(tracker,id,whenToStartTrackingTotalTps);
			this.target = target;
		}

		@Override
		public Splitter prepare(Splitter splitter, BufferedReader br) throws Exception {
			return target.prepare(splitter,br); 
		}

		@Override
		public void handle(Splitter splitter) throws Exception {
			exec(target,splitter);
		}
		
		@Override public CmdStats.Counter getStats()
		{
			return target.getStats();
		}
		
		@Override public void closeResources() throws Exception
		{
			target.closeResources();
		}
	}
	
	@Override public CmdStats.Counter getStats()
	{
		throw new UnsupportedOperationException("Recorder.getStats"); // shouldn't be called
	}
	
	@Override public void closeResources() throws Exception
	{
		throw new UnsupportedOperationException("Recorder.closeResources"); // shouldn't be called
	}
	
	@Override public Splitter prepare(Splitter splitter, BufferedReader br) throws Exception {throw new UnsupportedOperationException("Recorder.prepare");}
	@Override public void handle(Splitter splitter) throws Exception {throw new UnsupportedOperationException("Recorder.handle");}
	
	public Recorder(Tracker tracker, String id, long whenToStartTrackingTotalTps)
	{
		this.id = id;
		this.whenToStartTrackingTotalTps = whenToStartTrackingTotalTps;
		this.tracker = tracker;
		tracker.track(this);
	}
	
	public void start() throws Exception {}
	public void stop() throws Exception {}
	
	public void exec(Dispatchable target, Splitter splitter) throws Exception
	{
		synchronized (tracker) {
			final long before = System.currentTimeMillis();
			if (timeOfFirstCall == 0) {
				timeOfFirstCall = before;
			}
			try {
				target.handle(splitter);
			}
			catch (Exception e) {
				throw new Exception("Recorder target failure on count="+numberProcessedOverall,e);
			}
			finally {
				final long after = System.currentTimeMillis();
				final long callTime = after - before;

				numberProcessedSinceLastReport += 1;
				totalCallTimeSinceLastReport += callTime;
				if (++numberProcessedOverall > whenToStartTrackingTotalTps) {
					numberProcessedSinceTrackingStart++;
					totalCallTimeSinceTrackingStart += callTime;
				}
			}
		}
	}

	private static void write(FileWriter fw, long v) throws IOException
	{
		fw.write(',');
		fw.write(Long.toString(v));
	}

	public void writeHdr(FileWriter fw, String v) throws IOException
	{
		Tracker.writeEntry(fw,id+v);
	}

	@Override
	public final void reportHeader(FileWriter fw) throws IOException 
	{
		writeHdr(fw,"CountOverall");
		writeHdr(fw,"TpsOverall");
		writeHdr(fw,"AvgOverall");
		writeHdr(fw,"CountInterval");
		writeHdr(fw,"CountPerSecondInterval");
		writeHdr(fw,"TpsInterval");
		writeHdr(fw,"AvgInterval");
	}
	
	private static final long div(float x, float m, float y)
	{
		if (y == 0) return (long)x;
		return (long) ((x * m) / y);
	}

	@Override
	public final void reportData(FileWriter fw) throws IOException 
	{
		long tpsOverall = div(numberProcessedSinceTrackingStart,1000F,totalCallTimeSinceTrackingStart);
		long averageCallTimeOverall = div(totalCallTimeSinceTrackingStart,1F,numberProcessedSinceTrackingStart);
		long tpsSinceLastReport = div(numberProcessedSinceLastReport,1000F,totalCallTimeSinceLastReport);
		long averageCallTimeSinceLastReport = div(totalCallTimeSinceLastReport,1F,numberProcessedSinceLastReport);

		long time = System.currentTimeMillis();
		long totalElapsedTimeSinceLastReport = time - timeOfLastReport;
		long numberProcessedPerSecondSinceLastReport = div(numberProcessedSinceLastReport,1000F,totalElapsedTimeSinceLastReport);
		timeOfLastReport = time;
		
		/*
		if (totalCallTimeSinceLastReport == 0) {
			System.out.println("Recorder.reportData.ZERO, calls=" + numberProcessedSinceLastReport);
		}
		*/

		write(fw,numberProcessedOverall);
		write(fw,tpsOverall);
		write(fw,averageCallTimeOverall);			
		write(fw,numberProcessedSinceLastReport);
		write(fw,numberProcessedPerSecondSinceLastReport);
		write(fw,tpsSinceLastReport);
		write(fw,averageCallTimeSinceLastReport);
		numberProcessedSinceLastReport = 0;
		totalCallTimeSinceLastReport = 0;
	}
}

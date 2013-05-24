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

public class Meter 
{
	private final Dispatcher dispatcher;
	private final Recorder recorder;
	private final FixedBlockingQueue queue;
	
	private boolean inOperation = false;
	
	private CmdStats cmdCounter = new CmdStats();
	
	private int lastSetFrequency;
	private static final int RESOLUTION = 1000;
	private int[] schedule = new int[RESOLUTION];

	public class AtPercent {
		private final int scheduleIndex;
		private final int pct;
		private AtPercent(int pct, int index)
		{
			this.pct=pct; 
			this.scheduleIndex=index;
		}
		public Linear varyPerSecondTo(int target) {return new Linear(target,1000);}
		public Linear flat() {return new Linear(lastSetFrequency,1000);}
		public Flat flatPerSecond(int target) {return new Flat(target,1000);}

		public abstract class Variation {
			protected final int newIndex;
			public Variation()
			{
				this.newIndex = Math.min(pct * (RESOLUTION / 100), RESOLUTION-1);
			}
			public AtPercent atPercent(int pct) 
			{
				return new AtPercent(pct,newIndex);
			}
		}
		public class Flat extends Variation {
			public Flat(int target, int milliseconds)
			{
				super();
				target = tpsToMillisDelay(target);
				for (int i=scheduleIndex; i<=newIndex; i++) {
					//System.out.println("FLAT schedule["+i+"] = " + target);
					schedule[i] = target;
				}
				lastSetFrequency = target;
			}
		}
		public class Linear extends Variation {
			public Linear(int target, int milliseconds)
			{
				super();
				final int fullIncrease = target - lastSetFrequency;
				final int slots = newIndex - scheduleIndex;
				for (int i=0; i<slots; i++) {
					int currentIncrease = (int)(fullIncrease * (i/(float)slots));
					int delay = tpsToMillisDelay(lastSetFrequency + currentIncrease);
					//System.out.println("LINEAR schedule[("+scheduleIndex+"+"+i+")="+(scheduleIndex+i)+"] = " + delay);
					schedule[scheduleIndex+i] = delay;
				}
				lastSetFrequency = target;
			}
		}
	}

	private static int tpsToMillisDelay(int tps)
	{
		return tps<=0 ? -1 : (1000/tps);
	}
	
	public AtPercent atPercent(int pct) {return  new AtPercent(pct,0);}
	
	/**
	 * @param size max number of splitters to keep
	 * @param defaultFrequencyPerSecond how often to invoke target at start
	 * @param dispatcher
	 * @param recorder
	 */
	public Meter(int size, int defaultFrequencyPerSecond, Dispatcher dispatcher, Recorder recorder)
	{
		queue = new FixedBlockingQueue(size);
		this.recorder = recorder;
		this.dispatcher = dispatcher;

		lastSetFrequency = defaultFrequencyPerSecond;
		for (int i=0; i<RESOLUTION; i++) {
			schedule[i] = tpsToMillisDelay(defaultFrequencyPerSecond);
		}
	}

	public synchronized void stop()
	{
		inOperation = false;
	}

	public synchronized void start()
	{
		if (inOperation) return;
		inOperation = true;
		new Thread(new Runnable() {
			@Override public void run() {
				try {
					long pointBeforeCall = System.currentTimeMillis();
					while (inOperation) {
						delay(pointBeforeCall);
						MeteredDispatchable metered = queue.dequeue();
						//System.out.println("DQ " + inOperation);
						if (!inOperation) break; // May have been reset while/if dequeue() suspended
						pointBeforeCall = System.currentTimeMillis();
						
						try {
							//System.out.println("PRE");
							metered.execNow();
							//System.out.println("POST");
						}
						catch (Throwable cf) {
							logMeteredError(metered,cf);
						}
					}
				} 
				catch (Exception e) {
					System.err.println("Metering terminating due to: " + e.getMessage());
					inOperation = false;
					e.printStackTrace();
					System.exit(0);  // TBD, should probably coordinate with dispatcher
				}
			}
		}).start();
	}
	
	private static final int CHECK_FOR_EXIT_FREQUENCY_MILLIS = 500;
	
	//private long thing = -1;
	
	private void delay(long pointBeforeCall) throws InterruptedException
	{
		while (true) {
			int currentScheduleIndex = (int)(RESOLUTION * dispatcher.getCompletionPercentage());
			if (currentScheduleIndex >= RESOLUTION || !inOperation) {  // Done
				inOperation = false;
				break;
			}
			int delay = schedule[currentScheduleIndex];
			long timeToSleep = CHECK_FOR_EXIT_FREQUENCY_MILLIS;
			if (delay > 0) {
				final long pointAfterCall = System.currentTimeMillis();
				final long passed = pointAfterCall - pointBeforeCall;
				timeToSleep = delay - passed;
				//System.out.println("Sleep?("+delay+"-"+passed+") =" + timeToSleep+ ", schedule["+currentScheduleIndex+"]="+schedule[currentScheduleIndex]);			
				if (timeToSleep <= 0) {
					break;
				}
			}
			//System.out.println("schedule["+currentScheduleIndex+"]="+schedule[currentScheduleIndex]+" --> sleep(" + timeToSleep + ")");
			timeToSleep = Math.min(timeToSleep,CHECK_FOR_EXIT_FREQUENCY_MILLIS);
			//if (thing != timeToSleep) System.out.println("Sleep("+timeToSleep+"), currentScheduleIndex="+currentScheduleIndex+", pct="+dispatcher.getCompletionPercentage());
			//thing = timeToSleep;
			Thread.sleep(timeToSleep);
			//System.out.println("Wake after delay");			
		}
	}
	
	private void logMeteredError(MeteredDispatchable metered, Throwable cf) 
	{
		Throwable rootCause = cf;
		StringBuilder sb = new StringBuilder();
		sb.append(cf.getMessage());
		for (int i=4; rootCause.getCause() != null; i+=2) { 
			rootCause = rootCause.getCause();
			sb.append('\n');
			for (int j=0; j<i; j++) sb.append(' ');
			sb.append(rootCause.getMessage());
		}
		System.err.println("Metering exception:\n  input="+metered.splitter.getOrg()+"\n  error="+sb);
		cmdCounter.err(metered.src,metered.splitter.get(0),rootCause);
		cf.printStackTrace();
	}
	
	public Dispatchable add(final Dispatchable d)
	{
		return new MeteredDispatchable(d);
	}
	
	private class MeteredDispatchable implements Dispatchable
	{
		private Splitter splitter;
		private final Dispatchable target;
		private final MeteredDispatchable src;
		private String cmd;
		MeteredDispatchable(Dispatchable target)
		{
			this.target = target;
			this.src = this;
			//cmdCounter.ensureCounter(this,cmd);
		}
		MeteredDispatchable(MeteredDispatchable src)
		{
			this.splitter = src.splitter;
			this.target = src.target;
			this.cmd = src.cmd;
			this.src = src;
		}
		@Override public Splitter prepare(Splitter splitter, BufferedReader br) throws Exception
		{
			this.cmd = splitter.get(0);
			cmdCounter.ensureCounter(this,cmd);
			this.splitter = target.prepare(splitter,br); 
			return new Splitter();
		}
		@Override public void handle(Splitter splitter_ignore) 
		{
			MeteredDispatchable copy = new MeteredDispatchable(this);
			queue.queue(copy);
			Meter.this.start();
		}
		private void execNow() throws Exception
		{
			recorder.exec(target,splitter);
			cmdCounter.bump(src,cmd);
		}
		
		@Override public CmdStats.Counter getStats()
		{
			return cmdCounter.get(this);
		}
		@Override
		public void closeResources() throws Exception 
		{
			Meter.this.stop();
			target.closeResources();
		}
	}
	
	public class FixedBlockingQueue {
		public MeteredDispatchable[] qq;
		public int start;
		public int end;

		public FixedBlockingQueue(int size)
		{
			if (size <= 0) {
				throw new IllegalArgumentException("Splitter.Queue length must be greater than zero");
			}
			this.qq = new MeteredDispatchable[size];
		}

		// X .  - s e 
		// . X	- e s
		// s e  - e s
		// e s	- s e
		// 0 1
		public synchronized void queue(MeteredDispatchable splitter)
		{
			int pos = end++;
			if (end >= qq.length){
				end = 0;
			}
			if (end == start) {
				start++;
				if (start >= qq.length) {
					start = 0;
				}
			}
			qq[pos] = splitter;
			notify();
		}

		public synchronized MeteredDispatchable dequeue() throws InterruptedException
		{
			while (start == end) {
				wait(1000);
				if (!inOperation) {  // awake every so often to see if meter has terminated
					return null;
				}
			}
			MeteredDispatchable rez = qq[start++];
			if (start >= qq.length){
				start = 0;
			}
			return rez;
		}
	}
}

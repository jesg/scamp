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

import java.io.FileWriter;
import java.io.IOException;

public interface Trackable 
{
	public void reportHeader(FileWriter fw) throws IOException;
	public void reportData(FileWriter fw) throws IOException;
	
	public void start() throws Exception;
	public void stop() throws Exception;
	
	public static class Process implements Trackable
	{
		private final ProcessMonitor monitor;
		private final String id;
		public Process(Tracker tracker, String id, ProcessMonitor monitor)
		{
			tracker.track(this);
			this.id = id;
			this.monitor = monitor;
		}
		
		@Override public void start() throws Exception {monitor.start();}
		@Override public void stop() throws Exception {monitor.stop();}

		@Override
		public void reportHeader(FileWriter fw) throws IOException 
		{
			Tracker.writeEntry(fw,id+"Cpu");
		}

		@Override
		public void reportData(FileWriter fw) throws IOException 
		{
			Tracker.writeEntry(fw,String.valueOf(monitor.getValue()));			
		}
	}
}

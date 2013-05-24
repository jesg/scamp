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
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;


/**
 * Provides CPU usage data.
 * 
 * Currently Windows specific.
 * 
 * @author bmccart
 *
 */
public class ProcessMonitor 
{
	private String processName;
	private int value = 0;
	private Process process = null;
	
	public ProcessMonitor(String processName)
	{
		this.processName = processName;
	}
	
	/**
	 * Return CPU usage data for this monitor. Safe to call at any frequency as the value
	 * is refreshed independently at fixed intervals.
	 * 
	 * @return CPU usage data
	 */
	public int getValue()
	{
		return value;
	}
	
	/**
	 * Stop the sub-process. Only has effect if start() previously called.
	 */
	public void stop()
	{
		if (process != null) {
			process.destroy();
			process = null;
		}
	}
	
	/**
	 * Starts a subprocess that feeds us CPU updates at fixed intervals.
	 * No-op if already started.
	 * 
	 * @throws Exception
	 */
	public void start() throws Exception
	{
		if (process != null) return;
		
		new Thread(new Runnable() {
			public void run() {
				InputStream is = null;
				try {
					// Obviously needs to be modified for *nix.
					ProcessBuilder pb = new ProcessBuilder("typeperf","\\Process("+processName+")\\% Processor Time");
					process = pb.start();
					is = process.getInputStream();
					BufferedReader reader =
						new BufferedReader(new InputStreamReader(is));
					String nextLine;
					boolean first = true;
					while (((nextLine = reader.readLine()) != null) && process != null) {
						if (nextLine.trim().length()==0) continue;
						if (first) {
							first=false;
						}
						else {
							int ix = nextLine.indexOf(',');
							String sv = nextLine.substring(ix+2,nextLine.length()-1);
							value = (int)Double.parseDouble(sv);
						}
					}
				} catch (Exception e) {
					System.err.println("Tracking error: " + e);
					e.printStackTrace();
				} finally {
					try {
						if (is != null) is.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
		}).start();
	}
}

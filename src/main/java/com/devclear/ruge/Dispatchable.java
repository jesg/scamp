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

public interface Dispatchable {

	public Splitter prepare(Splitter splitter, BufferedReader br) throws Exception;

	public void handle(Splitter splitter) throws Exception;
	
	public void closeResources() throws Exception;
	
	public CmdStats.Counter getStats();
	
	public class Ignore implements Dispatchable {
		private final CmdStats.Counter stats;
		public Ignore(String cmd) {stats = CmdStats.makeEmpty(cmd);}
		@Override public Splitter prepare(Splitter splitter, BufferedReader br) {return splitter;}
		@Override public void handle(Splitter splitter) throws Exception {}
		@Override public CmdStats.Counter getStats() {return stats;}
		@Override public void closeResources() throws Exception {}
	}
}

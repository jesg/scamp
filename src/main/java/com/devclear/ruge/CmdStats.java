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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

public class CmdStats 
{
	private HashMap<Object,Counter> counter = new HashMap<Object,Counter>();	
	public static class Counter {
		final String cmd;
		private int execCount = 0;
		private int errCount = 0;
		private List<String> errTypes = null;
		public Counter(String cmd) {this.cmd = cmd;}
		public void bump() {execCount++;}
		public void err(Throwable t) 
		{
			errCount++;
			String cn = t.getClass().getName();
			cn = cn.substring(cn.lastIndexOf('.')+1);
			cn = cn.substring(cn.lastIndexOf('$')+1);
			if (errTypes == null) errTypes = new ArrayList<String>();
			if (!errTypes.contains(cn)) errTypes.add(cn);
		}
		public List<String> getErrTypes() {return errTypes;}
		public int getExecCount() {return execCount;}
		public int getErrCount() {return errCount;}
	}
	public static Counter makeEmpty(String cmd)
	{
		return new Counter(cmd);
	}
	
	public Counter ensureCounter(Object key, String cmd)
	{
		Counter ctr = counter.get(key);
		if (ctr == null) {
			ctr = new Counter(cmd);
			counter.put(key,ctr);
		}
		return ctr;
	}
	
	public void bump(Object key, String cmd) 
	{
		Counter ctr = ensureCounter(key,cmd);
		ctr.bump();
	}
	
	public void err(Object key, String cmd, Throwable t) 
	{
		Counter ctr = ensureCounter(key,cmd);
		ctr.err(t);
	}
	
	public Counter get(Object key)
	{
		return counter.get(key);
	}

	public Collection<Counter> getCounters()
	{
		return counter.values();
	}
}

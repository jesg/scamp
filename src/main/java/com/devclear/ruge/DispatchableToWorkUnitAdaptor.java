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

public abstract class DispatchableToWorkUnitAdaptor implements Dispatchable
{
	private Splitter splitter;
	private int columnIndex = 1;
	private WorkUnit workUnit;
	private CmdStats.Counter counter = null;
	
	public DispatchableToWorkUnitAdaptor(WorkUnit workUnit)
	{
		this.workUnit = workUnit;
	}
	
	@Override public CmdStats.Counter getStats()
	{
		return counter;
	}
	
	@Override public void closeResources() throws Exception
	{
		workUnit.cleanup(true);
	}
	
	@Override public Splitter prepare(Splitter splitter, BufferedReader br) throws Exception {return splitter;}
	
	protected abstract void afterCall() throws Exception;
	protected abstract void beforeCall() throws Exception;
	
	@Override
	final public void handle(Splitter splitter) throws Exception
	{
		this.splitter = splitter;
		
		if (counter == null) {
			String cmd = splitter.get(0);
			counter = CmdStats.makeEmpty(cmd);
		}
		counter.bump();

		workUnit.prepare();

		try {
			columnIndex = 1;
			beforeCall();
			workUnit.exec();
			verifyAll();
		}
		catch (Exception e) {
			throw new Exception("EXCEPTION on input: " + splitter.getOrg(),e);
		}
		catch (Throwable e) {
			throw new Exception("ERROR on input: " + splitter.getOrg(),e);
		}
		finally {
			workUnit.cleanup(false);
			splitter = null;  // just to be safe
		}
	}
	
	private Splitter currentComparisonLine = null;
	private boolean comparisonFailure;
	
	private void verifyAll() throws Exception
	{
		int splitIndex = splitter.getSplitIndex();
		int resultLineIndex = 0;
		Splitter[] lines = null;
		int[] matches = null;

		while (workUnit.next()) {
			if (splitIndex < 0) {
				throw new ComparisonFailure("Result returned where none was expected in input");  // TBD
			}
			if (lines == null) {
				lines = splitter.getNested(splitIndex);
				matches = new int[lines.length];
			}

			if (!compareNextResult(splitIndex,++resultLineIndex,lines,matches)) {
				throw new ComparisonFailure("No match for result line " + resultLineIndex + " in input: " + splitter.getOrg());
			}
		}
		if (lines != null) {
			for (int i=0; i<matches.length; i++) {
				if (matches[i] == 0) {
					throw new ComparisonFailure("No matching result from input: " + splitter.getOrg());
				}
			}
		}
	}
	
	private boolean compareNextResult(int splitIndex, int resultLineIndex, Splitter[] lines, int[] matches) throws Exception
	{
		for (int i=0; i<lines.length; i++) {
			if (matches[i] > 0 ) continue;
			currentComparisonLine = lines[i];
			columnIndex = 1;
			comparisonFailure = false;
			afterCall();
			if (!comparisonFailure) {
				matches[i] = resultLineIndex;
				return true;
			}
		}
		return false;
	}
	
	protected void setStringImmediate(String v) throws Exception
	{
		workUnit.setStringArg(columnIndex++,v);
	}
	
	protected String getString(int pos) 
	{
		return splitter.get(pos);
	}

	protected void setString(int pos) throws Exception
	{
		String v = splitter.get(pos);
		workUnit.setStringArg(columnIndex++,v);
	}

	protected void compareString(int pos) throws Exception
	{
		String exp = currentComparisonLine.get(pos);
		String got = workUnit.getStringResult(columnIndex++);
		verify(exp,got);
	}
	
	protected void setDate(int pos) throws Exception
	{
		String v = splitter.get(pos);
		long dv = Long.parseLong(v) * 1000;		
		workUnit.setDateArg(columnIndex++,dv);
	}

	protected void compareDate(int pos) throws Exception
	{
		String v = currentComparisonLine.get(pos);
		long exp = Long.parseLong(v) * 1000;		
		long got = workUnit.getDateResult(columnIndex++);
		verify(exp,got);
	}
	
	protected void setArray(int pos) throws Exception
	{
		Object[] v = splitter.getArray(pos,'|');
		workUnit.setArrayArg(columnIndex++,v);
	}

	protected void compareArray(int pos) throws Exception
	{
		Object[] exp = currentComparisonLine.getArray(pos,'|');
		Object[] got = workUnit.getArrayResult(columnIndex++);
		verify(exp,got);
	}
	
	private void verify(Object[] exp, Object[] got) throws ComparisonFailure
	{
		if (exp == null) {
			if (got != null) {
				comparisonFailure = true;
				//throw new ComparisonFailure("Expected null, got \"" + got+ "\"");
			}
		}
		else if (got == null) {
			comparisonFailure = true;
			//throw new ComparisonFailure("Got null, expected \"" + exp + "\"");
		}
		else {
			int expSize = exp.length;
			int gotSize = got.length;
			if (expSize != gotSize) {
				comparisonFailure = true;
				//throw new ComparisonFailure("Size mismatch\"" + expSize + "\", got \"" + gotSize + "\"");
			}
			match: for (int i=0; i< expSize; i++) {
				for (int j=0; j<gotSize; j++) {
					if (exp[i].equals(got[i])) {
						continue match;
					}
				}
				comparisonFailure = true;
				//throw new ComparisonFailure("Expected \"" + exp[i] + "\", but not found in result");
			}
		}
	}
	
	private void verify(Object exp, Object got) throws ComparisonFailure
	{
		if (exp == null) {
			if (got != null) {
				comparisonFailure = true;
				//throw new ComparisonFailure("Expected null, got \"" + got+ "\"");
			}
		}
		else if (got == null) {
			comparisonFailure = true;
			//throw new ComparisonFailure("Got null, expected \"" + exp + "\"");
		}
		else if (!exp.equals(got)) {
			comparisonFailure = true;
			//throw new ComparisonFailure("Expected \"" + exp + "\", got \"" + got + "\"");
		}
	}
	
	public class ComparisonFailure extends Exception
	{
		private static final long serialVersionUID = 1L;

		public ComparisonFailure(String msg)
		{
			super("Comparison Failed on " + workUnit.describe() + ": "+ msg);
		}
	}
}


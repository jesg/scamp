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
import java.util.ArrayList;

public class Splitter 
{
	private String org;
	private ArrayList<Object> vector;
	private int splitIndex = -1; // TBD multiple splits
	
	private final static char SEP = ',';
	
	//public Splitter replace() {return new Splitter();}
	
	public Splitter()
	{
		reinit();
	}
	
	private void reinit()
	{
		org = null;
		vector = new ArrayList<Object>();
		splitIndex = -1;
	}
	
	/*
	public Splitter freeze() 
	{
		Splitter rez = new Splitter();
		rez.org = this.org;
		rez.vector = this.vector;
		rez.splitIndex = this.splitIndex;
		reinit();
		return rez;
	}
	*/
	
	private static int parseTo(String s, ArrayList<Object> vector, char sep)
	{
		vector.clear();
		if (s == null) {
			System.out.println("Splitter.parseTo return -1");
			return -1;
		}
		final int len = s.length();
		for (int i=0, last=0; i<=len; i++) {
			if (i>=len || s.charAt(i) == SEP) {
				String next = i==last ? null : s.substring(last,i);
				vector.add(next);
				last = i+1;
			}
		}
		return vector.size();
	}
	
	public static void main(String[]argv)
	{
		ArrayList<Object> xxx = new ArrayList<Object>();
		System.out.println(parseTo("a",xxx,','));
		System.out.println(parseTo("a,",xxx,','));		
		System.out.println(parseTo("a,b",xxx,','));		
	}
	
	String getOrg()
	{
		return org;
	}
	
	public int parse(BufferedReader br) throws IOException
	{
		org = br.readLine();
		splitIndex = -1;
		return parseTo(org,vector,',');
	}
	
	public int parseBlock(int pos, BufferedReader br) throws IOException
	{
		splitIndex = pos;
		String v = get(pos);
		int numberOfLines = Integer.parseInt(v);
		Splitter[] lines = new Splitter[numberOfLines];
		for (int i=0; i<numberOfLines; i++) {
			//String next = br.readLine();
			//next = next.substring(next.indexOf(',')+1);
			lines[i] = new Splitter();
			lines[i].parse(br);
		}
		vector.set(pos,lines);
		return numberOfLines;
	}
	
	public int getSplitIndex()
	{
		return splitIndex;
	}
	
	
	public String get(int i)
	{
		Object v = vector.get(i);
		return v == null ? null : v.toString();
	}

	public int getInt(int i)
	{
		Object v = vector.get(i);
		return v == null ? 0 : Integer.parseInt(v.toString());
	}

	public Object[] getArray(int i, char sep)
	{
		String get = get(i);
		return get==null ? null : get.split("[" + sep + "]");  // TBO
	}
	
	public Splitter[] getNested(int i)
	{
		return (Splitter[])vector.get(i);
	}
}

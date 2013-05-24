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

import java.sql.Array;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;


public class DbWorkUnit implements WorkUnit 
{
	private final Connection conn;
	private final String spName;
	private final int argCount;
	private CallableStatement cs = null;
	private ResultSet rs = null;

	public DbWorkUnit(Connection conn, String spName, int argCount) 
	{
		this.conn = conn;
		this.spName = spName;
		this.argCount = argCount;
	}
	
	@Override
	public String describe() {return "DbWorkUnit(" + spName + ')';}
	
	@Override
	public void prepare() throws SQLException 
	{
		if (cs == null) {
			String fullName = "{call " + spName + "(";
			for (int i=0; i< argCount; i++) {
				if (i>0) fullName += ',';
				fullName += '?'; 
			}
			fullName += ")}";
			cs = conn.prepareCall(fullName);
			//System.out.println("PREPARE " + cs + ", " + fullName);
		}
	}

	@Override
	public void exec() throws Exception 
	{
		cs.execute();
		//boolean rez = cs.execute();
		//System.out.println("DbWorkUnit.exec() rez is " + rez);
	}

	@Override
	public void cleanup(boolean closeAll) throws SQLException
	{
		closeAll = true; // TBD, reviewing how to terminate on abend
		
		if (rs != null) {
			rs.close();
			rs = null;
		}
		if (closeAll && cs != null) {
			cs.close();
			cs = null;
		}
	}

	@Override
	public boolean next() throws SQLException {
		if (rs == null) {
			rs = cs.getResultSet();
		}
		//System.out.println("DbWorkUnit.next rs=" + rs);
		return rs != null && rs.next();
	}

	@Override
	public void setStringArg(int i, String s) throws SQLException 
	{
		cs.setString(i,s);
	}

	@Override
	public String getStringResult(int i) throws SQLException 
	{
		String rez = rs.getString(i); 
		return rez == null ? null : rez.trim();
	}

	@Override
	public void setDateArg(int i, long dateSeconds) throws SQLException 
	{
		//Date date = new Date(Long.parseLong(dateSeconds) * 1000);		
		Date date = new Date(dateSeconds);
		cs.setDate(i,date);
	}

	@Override
	public long getDateResult(int i) throws SQLException 
	{
		Date date = rs.getDate(i);
		return date.getTime();
	}

	@Override
	public void setArrayArg(int i, Object[] s) throws SQLException 
	{
		if (s == null) {
			cs.setNull(i,java.sql.Types.ARRAY);
		}
		else {
			Array sa = conn.createArrayOf("VARCHAR",s);
			cs.setArray(i,sa);
		}
	}

	@Override
	public Object[] getArrayResult(int i) throws SQLException 
	{
		Array agot = rs.getArray(i);
		Object[] got = (Object[])agot.getArray();
		
		return got;
	}
}




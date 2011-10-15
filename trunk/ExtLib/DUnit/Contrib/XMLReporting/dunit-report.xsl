<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
  XSLT Transformation for XML output of DUNIT (An XTreme testing 
  framework for Delphi programs, see http://dunit.sourceforge.net)

  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/
 
  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  author: Laurent Laffont <llaffont@altaiire.fr>
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="html"/>

  <xsl:template match="/">
	<xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="TestRun">
    <div id="header">
      <h1>DUnit - Test report</h1>
    </div>
    <div id="content">
      <blockquote>
        <table cellpadding="0" border="0" cellspacing="1" width="100%">
      	  <xsl:apply-templates/>
        </table>
      </blockquote>
    </div>
  </xsl:template>

  <xsl:template match="TestRun/TestSuite[1]">
    <tr class="tableheader">
      <td colspan="3"><xsl:value-of select="@name"/></td>
      <xsl:choose>
	<xsl:when test="@result='PASS'">
	  <td colspan="2" class="testpass"><xsl:value-of select="@result"/></td>
	</xsl:when>
	<xsl:when test="@result='FAILS'">
	  <td colspan="2" class="testfailure"><xsl:value-of select="@result"/></td>
	</xsl:when>
	<xsl:when test="@result='ERROR'">
	  <td colspan="2" class="testerror"><xsl:value-of select="@result"/></td>
	</xsl:when>
      </xsl:choose>
    </tr>
    <tr>
    </tr>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="TestSuite">
    <tr class="testsuite">
      <td colspan="5"><xsl:value-of select="@name"/></td>
    </tr>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="Test">
    <tr class="test">
      <xsl:choose>
	
	<xsl:when test="@result='PASS'">
	  <td colspan="4"><xsl:value-of select="@name"/></td>
          <td class="testpass"><xsl:value-of select="@result"/></td>
	</xsl:when>
	
	<xsl:when test="@result='FAILS'">
	  <td><xsl:value-of select="@name"/></td>
	  <xsl:apply-templates/>
          <td class="testfailure"><xsl:value-of select="@result"/></td>
	</xsl:when>
	
	<xsl:when test="@result='ERROR'">
	  <td><xsl:value-of select="@name"/></td>
	  <xsl:apply-templates/>
          <td class="testerror"><xsl:value-of select="@result"/></td>
	</xsl:when>
      
        </xsl:choose>
    </tr>
  </xsl:template>

  <xsl:template match="FailureType">
    <td><xsl:apply-templates/></td>
  </xsl:template>

  <xsl:template match="Location">
    <td><xsl:apply-templates/></td>
  </xsl:template>

  <xsl:template match="Message">
    <td><xsl:apply-templates/></td>
  </xsl:template>
  
  <xsl:template match="Statistics">
     <blockquote>
       <table width="300px">
         <tr>
	   <td colspan="2" class="statistics">Statistics</td>
	 </tr>
         <xsl:apply-templates/>
       </table>
    </blockquote>
  </xsl:template>

  <xsl:template match="Stat">
     <tr>
       <td class="statname" width="50px"><xsl:value-of select="@name" /></td>
       <td class="statvalue" width="50px"><xsl:value-of select="@result" /></td>
     </tr>
  </xsl:template>
</xsl:stylesheet>

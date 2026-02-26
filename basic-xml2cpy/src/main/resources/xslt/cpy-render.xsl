<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <!-- Root template -->
  <xsl:template match="/">
    <xsl:apply-templates select="//record"/>
  </xsl:template>

  <!-- Each COBOL field / record -->
  <xsl:template match="record">
    <!-- Level number -->
    <xsl:variable name="level" select="level"/>
    <xsl:value-of select="$level"/>
    <xsl:text> </xsl:text>

    <!-- Field name -->
    <xsl:value-of select="name"/>
    <xsl:text> </xsl:text>

    <!-- PIC clause if exists -->
    <xsl:choose>
      <xsl:when test="pic">
        <xsl:text>PIC </xsl:text>
        <xsl:value-of select="pic"/>
        <xsl:text> </xsl:text>
      </xsl:when>
      <xsl:otherwise/>
    </xsl:choose>

    <!-- Usage clause if exists -->
    <xsl:if test="usage">
      <xsl:text>USAGE </xsl:text>
      <xsl:value-of select="usage"/>
      <xsl:text> </xsl:text>
    </xsl:if>

    <!-- Occurs clause if exists -->
    <xsl:if test="occurs">
      <xsl:text>OCCURS </xsl:text>
      <xsl:value-of select="occurs"/>
      <xsl:text> TIMES</xsl:text>
    </xsl:if>

    <xsl:text>&#10;</xsl:text>

    <!-- Nested fields -->
    <xsl:apply-templates select="record"/>
  </xsl:template>

  <!-- Handling continuation lines -->
  <xsl:template match="continuation">
    <xsl:text>     </xsl:text> <!-- 5-space indentation -->
    <xsl:value-of select="text"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

</xsl:stylesheet>

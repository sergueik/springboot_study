<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- Template for root copybook -->
  <xsl:template match="/copybook">
    <xsl:apply-templates select="item"/>
  </xsl:template>

  <!-- Template for each item -->
  <xsl:template match="item">
    <!-- Level number -->
    <xsl:value-of select="@level"/>
    <xsl:text> </xsl:text>

    <!-- Field name -->
    <xsl:value-of select="@name"/>
    
    <!-- PIC clause -->
    <xsl:if test="@picture">
      <xsl:text> PIC </xsl:text>
      <xsl:value-of select="@picture"/>
    </xsl:if>

    <!-- USAGE clause -->
    <xsl:if test="@usage">
      <xsl:text> USAGE </xsl:text>
      <xsl:value-of select="@usage"/>
    </xsl:if>

    <!-- OCCURS clause (if exists in cb2xml) -->
    <xsl:if test="@occurs">
      <xsl:text> OCCURS </xsl:text>
      <xsl:value-of select="@occurs"/>
      <xsl:text> TIMES</xsl:text>
    </xsl:if>

    <!-- End line -->
    <xsl:text>&#10;</xsl:text>

    <!-- Recurse for nested items -->
    <xsl:apply-templates select="item"/>
  </xsl:template>

</xsl:stylesheet>

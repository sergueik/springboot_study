package com.yxrkt.agent.utils;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.By;

import java.util.ArrayList;
import java.util.List;

/**
 * Utility class for generating comprehensive element identification data
 * including CSS selectors, XPath expressions, and DOM hierarchy information.
 */
public class ElementIdentifier {
    
    /**
     * Generate a comprehensive CSS selector for the given element
     */
    public static String generateCssSelector(WebElement element, WebDriver driver) {
        try {
            if (driver instanceof JavascriptExecutor) {
                JavascriptExecutor js = (JavascriptExecutor) driver;
                
                // Try to generate a unique CSS selector using JavaScript
                String script = 
                    "function getCssSelector(element) {" +
                    "  if (element.id) return '#' + element.id;" +
                    "  if (element.className) {" +
                    "    var classes = element.className.split(/\\s+/).join('.');" +
                    "    if (classes && document.querySelectorAll('.' + classes).length === 1) {" +
                    "      return '.' + classes;" +
                    "    }" +
                    "  }" +
                    "  var path = [];" +
                    "  while (element.nodeType === Node.ELEMENT_NODE) {" +
                    "    var selector = element.nodeName.toLowerCase();" +
                    "    if (element.id) {" +
                    "      selector += '#' + element.id;" +
                    "      path.unshift(selector);" +
                    "      break;" +
                    "    } else {" +
                    "      var sib = element, nth = 1;" +
                    "      while (sib = sib.previousElementSibling) {" +
                    "        if (sib.nodeName.toLowerCase() == selector) nth++;" +
                    "      }" +
                    "      if (nth != 1) selector += ':nth-of-type(' + nth + ')';" +
                    "    }" +
                    "    path.unshift(selector);" +
                    "    element = element.parentNode;" +
                    "  }" +
                    "  return path.join(' > ');" +
                    "}" +
                    "return getCssSelector(arguments[0]);";
                
                Object result = js.executeScript(script, element);
                return result != null ? result.toString() : "unknown";
            }
        } catch (Exception e) {
            // Fallback to basic selector generation
        }
        
        return generateBasicCssSelector(element);
    }
    
    /**
     * Generate a basic CSS selector as fallback
     */
    private static String generateBasicCssSelector(WebElement element) {
        try {
            String id = element.getAttribute("id");
            if (id != null && !id.isEmpty()) {
                return "#" + id;
            }
            
            String className = element.getAttribute("class");
            if (className != null && !className.isEmpty()) {
                return "." + className.trim().replaceAll("\\s+", ".");
            }
            
            return element.getTagName();
        } catch (Exception e) {
            return "unknown";
        }
    }
    
    /**
     * Generate XPath expression for the element
     */
    public static String generateXPath(WebElement element, WebDriver driver) {
        try {
            if (driver instanceof JavascriptExecutor) {
                JavascriptExecutor js = (JavascriptExecutor) driver;
                
                String script = 
                    "function getXPath(element) {" +
                    "  if (element.id !== '') return '//*[@id=\"' + element.id + '\"]';" +
                    "  if (element === document.body) return '/html/body';" +
                    "  var ix = 0;" +
                    "  var siblings = element.parentNode.childNodes;" +
                    "  for (var i = 0; i < siblings.length; i++) {" +
                    "    var sibling = siblings[i];" +
                    "    if (sibling === element) {" +
                    "      return getXPath(element.parentNode) + '/' + element.tagName.toLowerCase() + '[' + (ix + 1) + ']';" +
                    "    }" +
                    "    if (sibling.nodeType === 1 && sibling.tagName === element.tagName) ix++;" +
                    "  }" +
                    "}" +
                    "return getXPath(arguments[0]);";
                
                Object result = js.executeScript(script, element);
                return result != null ? result.toString() : "unknown";
            }
        } catch (Exception e) {
            // Fallback
        }
        
        return "//" + element.getTagName();
    }
    
    /**
     * Get comprehensive element attributes for identification
     */
    public static ElementInfo getElementInfo(WebElement element, WebDriver driver) {
        ElementInfo info = new ElementInfo();
        
        try {
            info.tagName = element.getTagName();
            info.id = element.getAttribute("id");
            info.className = element.getAttribute("class");
            info.name = element.getAttribute("name");
            info.type = element.getAttribute("type");
            info.value = element.getAttribute("value");
            info.placeholder = element.getAttribute("placeholder");
            info.title = element.getAttribute("title");
            info.alt = element.getAttribute("alt");
            info.href = element.getAttribute("href");
            info.src = element.getAttribute("src");
            info.role = element.getAttribute("role");
            info.ariaLabel = element.getAttribute("aria-label");
            info.dataTestId = element.getAttribute("data-testid");
            info.dataTest = element.getAttribute("data-test");
            info.text = element.getText();
            info.isDisplayed = element.isDisplayed();
            info.isEnabled = element.isEnabled();
            info.isSelected = element.isSelected();
            
            // Generate selectors
            info.cssSelector = generateCssSelector(element, driver);
            info.xpath = generateXPath(element, driver);
            
            // Get position and size
            try {
                info.location = element.getLocation().toString();
                info.size = element.getSize().toString();
            } catch (Exception ignored) {}
            
        } catch (Exception e) {
            // Handle any exceptions gracefully
        }
        
        return info;
    }
    
    /**
     * Data class to hold comprehensive element information
     */
    public static class ElementInfo {
        public String tagName;
        public String id;
        public String className;
        public String name;
        public String type;
        public String value;
        public String placeholder;
        public String title;
        public String alt;
        public String href;
        public String src;
        public String role;
        public String ariaLabel;
        public String dataTestId;
        public String dataTest;
        public String text;
        public String cssSelector;
        public String xpath;
        public String location;
        public String size;
        public boolean isDisplayed;
        public boolean isEnabled;
        public boolean isSelected;
    }
}
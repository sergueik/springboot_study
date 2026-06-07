package com.yxrkt.agent.hooks;

import net.bytebuddy.asm.Advice;
import com.yxrkt.agent.bus.EventBus;
import com.yxrkt.agent.utils.ElementIdentifier;
import com.yxrkt.agent.error.ErrorHandler;

import org.openqa.selenium.*;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.openqa.selenium.WrapsDriver;

import java.util.Arrays;
import java.util.regex.Pattern;

public class SeleniumInterceptors {
    public static String driverId(Object maybeElementOrDriver) {
        try {
            RemoteWebDriver rwd = null;
            if (maybeElementOrDriver instanceof RemoteWebDriver) rwd = (RemoteWebDriver) maybeElementOrDriver;
            if (maybeElementOrDriver instanceof WrapsDriver)
                rwd = (RemoteWebDriver) ((WrapsDriver) maybeElementOrDriver).getWrappedDriver();
            return (rwd != null) ? Integer.toHexString(System.identityHashCode(rwd)) : "na";
        } catch (Throwable t) { return "na"; }
    }

    public static class WebElementNoArgs {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This WebElement el, @Advice.Origin("#m") String m) {
            return EventBus.bus().startStep(m, driverId(el), getCleanLocator(el), null);
        }
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This WebElement el, @Advice.Origin("#m") String m,
                           @Advice.Thrown Throwable t) {
            EventBus.bus().endStep(start, m, driverId(el), getCleanLocator(el), t);
        }
    }

public static class WebElementSendKeys {
    private static final Pattern SENSITIVE_LOCATOR = Pattern.compile("(?i)(password|pwd|pass)");

        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This WebElement el, @Advice.AllArguments Object[] args) {
            System.out.println("[CDP Agent] WebElement.sendKeys interceptor triggered on " + el.getClass().getName());
            String meta = Arrays.toString(args);
            String inputText = meta;
            try {
                String locator = getCleanLocator(el);
                if (SENSITIVE_LOCATOR.matcher(locator).find()) {
                    inputText = "[REDACTED]";
                }
            } catch (Throwable ignored) {
                // Keep original meta if locator check fails
            }
            final String finalInputText = inputText;
            
            // Capture comprehensive interaction data using ElementIdentifier
            // EventBus.bus().cdp("WebElement.sendKeys.start", el, j -> {
            //     j.add("action", "sendKeys");
            //     j.add("inputText", finalInputText);
            //     j.add("elementLocator", el.toString());
            //     
            //     // Get comprehensive element information
            //     try {
            //         WebDriver driver = ((WrapsDriver) el).getWrappedDriver();
            //         ElementIdentifier.ElementInfo info = ElementIdentifier.getElementInfo(el, driver);
            //         
            //         // Add all element identification data
            //         j.add("tagName", info.tagName);
            //         j.add("id", info.id);
            //         j.add("name", info.name);
            //         j.add("class", info.className);
            //         j.add("type", info.type);
            //         j.add("placeholder", info.placeholder);
            //         j.add("value", info.value);
            //         j.add("isDisplayed", info.isDisplayed);
            //         j.add("isEnabled", info.isEnabled);
            //         
            //         // Add generated selectors for better element identification
            //         j.add("cssSelector", info.cssSelector);
            //         j.add("xpath", info.xpath);
            //     } catch (Throwable ignored) {
            //         // Fallback to basic element data if ElementIdentifier fails
            //         try {
            //             j.add("tagName", el.getTagName());
            //             j.add("id", el.getAttribute("id"));
            //             j.add("class", el.getAttribute("class"));
            //             j.add("type", el.getAttribute("type"));
            //         } catch (Throwable ignored2) {}
            //     }
            //     return null;
            // });
            
            return EventBus.bus().startStep("sendKeys", driverId(el), getCleanLocator(el), finalInputText);
        }
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This WebElement el, @Advice.Thrown Throwable t) {
            EventBus.bus().endStep(start, "sendKeys", driverId(el), getCleanLocator(el), t);
        }
    }

    public static class WebDriverGet {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This WebDriver d, @Advice.AllArguments Object[] args) {
            System.out.println("[CDP Agent] WebDriver.get interceptor triggered on " + d.getClass().getName());
            String targetUrl = args.length > 0 ? args[0].toString() : "unknown";
            
            // Capture comprehensive navigation data
            try {
                if (EventBus.bus() == null) {
                    System.out.println("[CDP Agent] EventBus is null in WebDriver.get!");
                    return 0L;
                }
                System.out.println("[CDP Agent] Writing WebDriver.get event");
                System.out.println("[CDP Agent] EventBus instance: " + EventBus.bus());
                System.out.println("[CDP Agent] About to call EventBus.directEmit for WebDriver.get.start");
                System.out.println("[CDP Agent] EventBus instance hashcode: " + System.identityHashCode(EventBus.bus()));
                System.out.flush();
                
                // Bypass CDP enrichment and directly emit event to avoid deadlock
                try {
                    System.out.println("[CDP Agent] Starting directEmit call for WebDriver.get");
                    long start = System.currentTimeMillis();
                    System.out.println("[CDP Agent] Calling EventBus.bus().directEmit now...");
                    EventBus.bus().directEmit("WebDriver.get.start", d, null);
                    System.out.println("[CDP Agent] WebDriver.get directEmit call completed");
                    System.out.println("[CDP Agent] WebDriver.get directEmit call returned");
                    System.out.flush();
                    System.out.println("[CDP Agent] WebDriver.get event written successfully");
                } catch (Exception cdpEx) {
                    System.out.println("[CDP Agent] DirectEmit call failed for WebDriver.get: " + cdpEx.getMessage());
                    cdpEx.printStackTrace();
                } catch (Throwable t) {
                    System.out.println("[CDP Agent] Throwable in directEmit for WebDriver.get: " + t.getMessage());
                    t.printStackTrace();
                }
            } catch (Exception e) {
                System.out.println("[CDP Agent] Error writing WebDriver.get event: " + e.getMessage());
            }
            
            return EventBus.bus().startStep("get", driverId(d), Arrays.toString(args), null);
        }
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This WebDriver d, @Advice.Thrown Throwable t) {
            try {
                // Emit the corresponding end event
                EventBus.bus().directEmit("WebDriver.get.end", d, null);
                System.out.println("[CDP Agent] WebDriver.get.end event emitted");
                
                // Capture navigation completion with page context
                // EventBus.bus().cdp("WebDriver.get.completed", d, j -> {
                //     j.add("action", "get");
                //     j.add("success", t == null);
                //     j.add("finalUrl", getCurrentUrlSafely(d));
                //     j.add("pageTitle", getPageTitleSafely(d));
                //     j.add("pageSource", getPageSourceSafely(d));
                //     j.add("duration", System.currentTimeMillis() - start);
                //     if (t != null) {
                //         j.add("error", t.getClass().getSimpleName());
                //         j.add("errorMessage", t.getMessage());
                //     }
                //     return null;
                // });
            } catch (Exception e) {
                System.out.println("[CDP Agent] Error in WebDriver.get exit: " + e.getMessage());
            }
            
            EventBus.bus().endStep(start, "get", driverId(d), d.getCurrentUrl(), t);
        }
        
        private static String getCurrentUrlSafely(WebDriver d) {
            try { return d.getCurrentUrl(); } catch (Throwable t) { return "unknown"; }
        }
        
        private static String getPageTitleSafely(WebDriver d) {
            try { return d.getTitle(); } catch (Throwable t) { return "unknown"; }
        }
        
        private static String getWindowHandleSafely(WebDriver d) {
            try { return d.getWindowHandle(); } catch (Throwable t) { return "unknown"; }
        }
        
        private static String getWindowHandlesSafely(WebDriver d) {
            try { return d.getWindowHandles().toString(); } catch (Throwable t) { return "unknown"; }
        }
        
        private static String getPageSourceSafely(WebDriver d) {
            try { 
                String source = d.getPageSource();
                // Truncate page source to avoid huge events
                return source.length() > 1000 ? source.substring(0, 1000) + "..." : source;
            } catch (Throwable t) { return "unknown"; }
        }
    }

    public static class Finders {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This Object ctx, @Advice.Origin("#m") String m, @Advice.AllArguments Object[] args) {
            By locator = args.length > 0 ? (By) args[0] : null;
            String locatorString = locator != null ? locator.toString() : "unknown";
            
            // Capture comprehensive element finding data
            // EventBus.bus().cdp("WebDriver." + m + ".start", ctx, j -> {
            //     j.add("action", m);
            //     j.add("locator", locatorString);
            //     j.add("locatorType", extractLocatorType(locatorString));
            //     j.add("locatorValue", extractLocatorValue(locatorString));
            //     j.add("contextType", ctx.getClass().getSimpleName());
            //     j.add("contextString", ctx.toString());
            //     
            //     // Add page context if available
            //     if (ctx instanceof WebDriver) {
            //         WebDriver driver = (WebDriver) ctx;
            //         j.add("currentUrl", getCurrentUrlSafely(driver));
            //         j.add("pageTitle", getPageTitleSafely(driver));
            //     }
            //     return null;
            // });
            
            return EventBus.bus().startStep(m, driverId(ctx), "searchContext", Arrays.toString(args));
        }
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This Object ctx, @Advice.Origin("#m") String m,
                           @Advice.Thrown Throwable t, @Advice.Return Object result) {
            // Capture element finding completion with results
            // EventBus.bus().cdp("WebDriver." + m + ".completed", ctx, j -> {
            //     j.add("action", m);
            //     j.add("success", t == null);
            //     j.add("duration", System.currentTimeMillis() - start);
            //     
            //     if (t == null && result != null) {
            //         if (result instanceof WebElement) {
            //             WebElement element = (WebElement) result;
            //             j.add("elementFound", true);
            //             j.add("elementCount", 1);
            //             
            //             // Get comprehensive element information using ElementIdentifier
            //             try {
            //                 WebDriver driver = ctx instanceof WebDriver ? (WebDriver) ctx : 
            //                                   ((WrapsDriver) element).getWrappedDriver();
            //                 ElementIdentifier.ElementInfo info = ElementIdentifier.getElementInfo(element, driver);
            //                 
            //                 j.add("foundElementTag", info.tagName);
            //                 j.add("foundElementId", info.id);
            //                 j.add("foundElementClass", info.className);
            //                 j.add("foundElementText", info.text);
            //                 j.add("foundElementType", info.type);
            //                 j.add("foundElementName", info.name);
            //                 j.add("foundElementCssSelector", info.cssSelector);
            //                 j.add("foundElementXPath", info.xpath);
            //             } catch (Throwable ignored) {
            //                 // Fallback to basic element data
            //                 try {
            //                     j.add("foundElementTag", element.getTagName());
            //                     j.add("foundElementId", element.getAttribute("id"));
            //                     j.add("foundElementClass", element.getAttribute("class"));
            //                     j.add("foundElementText", element.getText());
            //                 } catch (Throwable ignored2) {}
            //             }
            //         } else if (result instanceof java.util.List) {
            //             java.util.List<?> elements = (java.util.List<?>) result;
            //             j.add("elementFound", !elements.isEmpty());
            //             j.add("elementCount", elements.size());
            //             
            //             // For multiple elements, capture info about the first element
            //             if (!elements.isEmpty() && elements.get(0) instanceof WebElement) {
            //                 try {
            //                     WebElement firstElement = (WebElement) elements.get(0);
            //                     WebDriver driver = ctx instanceof WebDriver ? (WebDriver) ctx : 
            //                                       ((WrapsDriver) firstElement).getWrappedDriver();
            //                     ElementIdentifier.ElementInfo info = ElementIdentifier.getElementInfo(firstElement, driver);
            //                     
            //                     j.add("firstElementTag", info.tagName);
            //                     j.add("firstElementId", info.id);
            //                     j.add("firstElementClass", info.className);
            //                     j.add("firstElementCssSelector", info.cssSelector);
            //                 } catch (Throwable ignored) {}
            //             }
            //         }
            //     } else {
            //         j.add("elementFound", false);
            //         j.add("elementCount", 0);
            //         if (t != null) {
            //             j.add("error", t.getClass().getSimpleName());
            //             j.add("errorMessage", t.getMessage());
            //         }
            //     }
            //     return null;
            // });
            
            EventBus.bus().endStep(start, m, driverId(ctx), "searchContext", t);
        }
        
        private static String extractLocatorType(String locatorString) {
            if (locatorString.startsWith("By.id:")) return "id";
            if (locatorString.startsWith("By.className:")) return "className";
            if (locatorString.startsWith("By.cssSelector:")) return "cssSelector";
            if (locatorString.startsWith("By.xpath:")) return "xpath";
            if (locatorString.startsWith("By.name:")) return "name";
            if (locatorString.startsWith("By.tagName:")) return "tagName";
            if (locatorString.startsWith("By.linkText:")) return "linkText";
            if (locatorString.startsWith("By.partialLinkText:")) return "partialLinkText";
            return "unknown";
        }
        
        private static String extractLocatorValue(String locatorString) {
            int colonIndex = locatorString.indexOf(":");
            if (colonIndex > 0 && colonIndex < locatorString.length() - 1) {
                return locatorString.substring(colonIndex + 1).trim();
            }
            return locatorString;
        }
        
        private static String getCurrentUrlSafely(WebDriver d) {
            try { return d.getCurrentUrl(); } catch (Throwable t) { return "unknown"; }
        }
        
        private static String getPageTitleSafely(WebDriver d) {
            try { return d.getTitle(); } catch (Throwable t) { return "unknown"; }
        }
    }

    public static class JsExec {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This Object d, @Advice.AllArguments Object[] args) {
            String script = args!=null && args.length>0 ? String.valueOf(args[0]) : "";
            return EventBus.bus().startStep("executeScript", driverId(d), "driver", script.length()<=200?script:script.substring(0,200));
        }
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This Object d, @Advice.Thrown Throwable t) {
            EventBus.bus().endStep(start, "executeScript", driverId(d), "driver", t);
        }
    }

    public static class ActionsPerform {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This Object actions) {
            return EventBus.bus().startStep("actions.perform", driverId(actions), "actions", null);
        }
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This Object actions, @Advice.Thrown Throwable t) {
            EventBus.bus().endStep(start, "actions.perform", driverId(actions), "actions", t);
        }
    }

    public static class WebDriverNavigation {
        @Advice.OnMethodEnter
        static long onEnter(@Advice.This Object navigation, @Advice.Origin("#m") String method, @Advice.AllArguments Object[] args) {
            System.out.println("[CDP Agent] WebDriver.Navigation interceptor triggered: " + method);
            try {
                System.out.println("[CDP Agent] Navigation object class: " + navigation.getClass().getName());
                WebDriver driver;
                try {
                    driver = getDriverFromNavigation(navigation);
                    System.out.println("[CDP Agent] Navigation driver extracted: " + (driver != null ? driver.getClass().getSimpleName() : "null"));
                } catch (Exception driverEx) {
                    System.out.println("[CDP Agent] Exception extracting driver from navigation: " + driverEx.getMessage());
                    driverEx.printStackTrace();
                    return System.currentTimeMillis();
                }
                if (driver == null) {
                    System.out.println("[CDP Agent] Driver is null, skipping navigation event");
                    return System.currentTimeMillis();
                }
                String driverId = driverId(driver);
                System.out.println("[CDP Agent] Navigation driverId: " + driverId);
                
                System.out.println("[CDP Agent] EventBus.bus() is " + (EventBus.bus() == null ? "null" : "not null"));
                if (EventBus.bus() != null) {
                    System.out.println("[CDP Agent] About to call EventBus.directEmit for Navigation." + method + ".start");
                    try {
                        EventBus.bus().directEmit("Navigation." + method + ".start", navigation, null);
                        System.out.println("[CDP Agent] Navigation directEmit call completed: " + method);
                    } catch (Exception e) {
                        System.out.println("[CDP Agent] DirectEmit call failed for Navigation: " + e.getMessage());
                        e.printStackTrace();
                    }
                } else {
                    System.out.println("[CDP Agent] EventBus.bus() is null, skipping directEmit for Navigation." + method);
                }
                
                // Capture navigation context
                try {
                    // EventBus.bus().cdp("Navigation." + method + ".start", navigation, j -> {
                    //     j.add("action", method);
                    //     j.add("currentUrl", getCurrentUrlSafely(driver));
                    //     j.add("pageTitle", getPageTitleSafely(driver));
                    //     j.add("windowHandle", getWindowHandleSafely(driver));
                    //     if (args != null && args.length > 0 && "to".equals(method)) {
                    //         j.add("targetUrl", args[0].toString());
                    //     }
                    //     return null;
                    // });
                } catch (Exception cdpEx) {
                    System.out.println("[CDP Agent] Failed to emit navigation start CDP event: " + cdpEx.getMessage());
                }
                
                return EventBus.bus().startStep("Navigation." + method, driverId, navigation.toString(), null);
            } catch (Exception e) {
                System.out.println("[CDP Agent] Error in Navigation interceptor: " + e.getMessage());
                return 0L;
            }
        }
        
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This Object navigation, @Advice.Origin("#m") String method, @Advice.Thrown Throwable t) {
            try {
                WebDriver driver = getDriverFromNavigation(navigation);
                
                // Capture navigation completion with new page context
                try {
                    com.eclipsesource.json.JsonObject navCompletion = new com.eclipsesource.json.JsonObject();
                    navCompletion.add("action", method);
                    navCompletion.add("success", t == null);
                    if (t != null) {
                        navCompletion.add("error", t.getClass().getSimpleName());
                        navCompletion.add("errorMessage", t.getMessage());
                    }
                    navCompletion.add("newUrl", getCurrentUrlSafely(driver));
                    navCompletion.add("newPageTitle", getPageTitleSafely(driver));
                    navCompletion.add("duration", System.currentTimeMillis() - start);
                    EventBus.bus().cdp("Navigation." + method + ".completed", navigation, navCompletion);
                } catch (Exception cdpEx) {
                    System.out.println("[CDP Agent] Failed to emit navigation completion CDP event: " + cdpEx.getMessage());
                }
                
                EventBus.bus().endStep(start, "Navigation." + method, driverId(driver), navigation.toString(), t);
            } catch (Exception e) {
                System.out.println("[CDP Agent] Error in Navigation exit: " + e.getMessage());
            }
        }
        
        public static WebDriver getDriverFromNavigation(Object navigation) {
            System.out.println("[CDP Agent] Attempting to extract driver from navigation object");
            try {
                // For inner classes like RemoteNavigation, try to access the outer instance via 'this$0'
                System.out.println("[CDP Agent] Trying 'this$0' field for inner class outer instance");
                java.lang.reflect.Field outerField = navigation.getClass().getDeclaredField("this$0");
                outerField.setAccessible(true);
                Object outerInstance = outerField.get(navigation);
                if (outerInstance instanceof WebDriver) {
                    WebDriver result = (WebDriver) outerInstance;
                    System.out.println("[CDP Agent] Successfully extracted driver via 'this$0' field: " + (result != null ? result.getClass().getSimpleName() : "null"));
                    return result;
                }
                System.out.println("[CDP Agent] Outer instance is not a WebDriver: " + (outerInstance != null ? outerInstance.getClass().getSimpleName() : "null"));
            } catch (Exception e) {
                System.out.println("[CDP Agent] Failed to get 'this$0' field: " + e.getMessage());
            }
            
            // Fallback: try traditional field names
            try {
                System.out.println("[CDP Agent] Trying 'driver' field");
                java.lang.reflect.Field driverField = navigation.getClass().getDeclaredField("driver");
                driverField.setAccessible(true);
                WebDriver result = (WebDriver) driverField.get(navigation);
                System.out.println("[CDP Agent] Successfully extracted driver via 'driver' field: " + (result != null ? result.getClass().getSimpleName() : "null"));
                return result;
            } catch (Exception e) {
                System.out.println("[CDP Agent] Failed to get 'driver' field: " + e.getMessage());
                // Fallback: try to get driver from parent field
                try {
                    System.out.println("[CDP Agent] Trying 'parent' field");
                    java.lang.reflect.Field parentField = navigation.getClass().getDeclaredField("parent");
                    parentField.setAccessible(true);
                    WebDriver result = (WebDriver) parentField.get(navigation);
                    System.out.println("[CDP Agent] Successfully extracted driver via 'parent' field: " + (result != null ? result.getClass().getSimpleName() : "null"));
                    return result;
                } catch (Exception ex) {
                    System.out.println("[CDP Agent] Failed to get 'parent' field: " + ex.getMessage());
                    System.out.println("[CDP Agent] Could not extract driver from navigation: " + ex.getMessage());
                    return null;
                }
            }
        }
        
        public static String getCurrentUrlSafely(WebDriver d) {
            try { return d != null ? d.getCurrentUrl() : "unknown"; } catch (Exception e) { return "error"; }
        }
        
        public static String getPageTitleSafely(WebDriver d) {
            try { return d != null ? d.getTitle() : "unknown"; } catch (Exception e) { return "error"; }
        }
        
        public static String getWindowHandleSafely(WebDriver d) {
            try { return d != null ? d.getWindowHandle() : "unknown"; } catch (Exception e) { return "error"; }
        }
    }

    public static class WebDriverWindow {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This Object targetLocator, @Advice.Origin("#m") String method, @Advice.AllArguments Object[] args) {
            System.out.println("[CDP Agent] WebDriver.TargetLocator interceptor triggered: " + method);
            try {
                WebDriver driver = getDriverFromTargetLocator(targetLocator);
                String driverId = driverId(driver);
                
                if (EventBus.bus() != null) {
                    System.out.println("[CDP Agent] About to call EventBus.directEmit for Window." + method + ".start");
                    try {
                        EventBus.bus().directEmit("Window." + method + ".start", targetLocator, null);
                        System.out.println("[CDP Agent] Window directEmit call completed: " + method);
                    } catch (Exception e) {
                        System.out.println("[CDP Agent] DirectEmit call failed for Window: " + e.getMessage());
                    }
                }
                
                // Capture window context
                // EventBus.bus().cdp("Window." + method + ".start", targetLocator, j -> {
                //     j.add("action", method);
                //     j.add("currentUrl", getCurrentUrlSafely(driver));
                //     j.add("currentWindowHandle", getWindowHandleSafely(driver));
                //     j.add("allWindowHandles", getWindowHandlesSafely(driver));
                //     if (args != null && args.length > 0) {
                //         if ("window".equals(method)) {
                //             j.add("targetWindow", args[0].toString());
                //         } else if ("frame".equals(method)) {
                //             j.add("targetFrame", args[0].toString());
                //         }
                //     }
                //     return null;
                // });
                
                return EventBus.bus().startStep("Window." + method, driverId, targetLocator.toString(), null);
            } catch (Exception e) {
                System.out.println("[CDP Agent] Error in Window interceptor: " + e.getMessage());
                return 0L;
            }
        }
        
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This Object targetLocator, @Advice.Origin("#m") String method, @Advice.Thrown Throwable t) {
            try {
                WebDriver driver = getDriverFromTargetLocator(targetLocator);
                
                // Capture window switch completion
                // EventBus.bus().cdp("Window." + method + ".completed", targetLocator, j -> {
                //     j.add("action", method);
                //     j.add("success", t == null);
                //     if (t != null) {
                //         j.add("error", t.getClass().getSimpleName());
                //         j.add("errorMessage", t.getMessage());
                //     }
                //     j.add("newUrl", getCurrentUrlSafely(driver));
                //     j.add("newWindowHandle", getWindowHandleSafely(driver));
                //     j.add("duration", System.currentTimeMillis() - start);
                //     return null;
                // });
                
                EventBus.bus().endStep(start, "Window." + method, driverId(driver), targetLocator.toString(), t);
            } catch (Exception e) {
                System.out.println("[CDP Agent] Error in Window exit: " + e.getMessage());
            }
        }
        
        private static WebDriver getDriverFromTargetLocator(Object targetLocator) {
            try {
                // TargetLocator objects typically have a reference to the driver
                java.lang.reflect.Field driverField = targetLocator.getClass().getDeclaredField("driver");
                driverField.setAccessible(true);
                return (WebDriver) driverField.get(targetLocator);
            } catch (Exception e) {
                // Fallback: try common field names
                try {
                    java.lang.reflect.Field parentField = targetLocator.getClass().getDeclaredField("parent");
                    parentField.setAccessible(true);
                    return (WebDriver) parentField.get(targetLocator);
                } catch (Exception ex) {
                    System.out.println("[CDP Agent] Could not extract driver from targetLocator: " + ex.getMessage());
                    return null;
                }
            }
        }
        
        private static String getCurrentUrlSafely(WebDriver d) {
            try { return d != null ? d.getCurrentUrl() : "unknown"; } catch (Throwable t) { return "unknown"; }
        }
        
        private static String getWindowHandleSafely(WebDriver d) {
            try { return d != null ? d.getWindowHandle() : "unknown"; } catch (Throwable t) { return "unknown"; }
        }
        
        private static String getWindowHandlesSafely(WebDriver d) {
            try { return d != null ? d.getWindowHandles().toString() : "unknown"; } catch (Throwable t) { return "unknown"; }
        }
    }
    
    // Shared utility methods for all interceptors
    private static String getUrlSafely(WebDriver d) {
        try { return d != null ? d.getCurrentUrl() : "unknown"; } catch (Throwable t) { return "unknown"; }
    }

    // WebDriverWait and FluentWait interceptors for timing and synchronization
    public static class WebDriverWaitUntil {
        @Advice.OnMethodEnter
        static long onEnter(@Advice.This Object wait, @Advice.AllArguments Object[] args) {
            System.out.println("[CDP Agent] WebDriverWait.until interceptor triggered");
            System.out.println("[CDP Agent] EventBus.bus() is: " + EventBus.bus());
            try {
                System.out.println("[CDP Agent] Getting driver from wait object");
                WebDriver driver = getDriverFromWait(wait);
                System.out.println("[CDP Agent] Driver obtained: " + (driver != null ? driver.getClass().getSimpleName() : "null"));
                String waitType = wait.getClass().getSimpleName();
                String conditionType = args.length > 0 ? args[0].getClass().getSimpleName() : "unknown";
                System.out.println("[CDP Agent] Wait type: " + waitType + ", Condition type: " + conditionType);
                
                if (EventBus.bus() != null) {
                    System.out.println("[CDP Agent] About to call EventBus.directEmit for Wait.until.start");
                    EventBus.bus().directEmit("Wait.until.start", wait, null);
                    System.out.println("[CDP Agent] Wait.until directEmit call completed");
                    
                    // Capture wait context with timing details
                    // EventBus.bus().cdp("Wait.until.start", wait, j -> {
                    //     j.add("waitType", waitType);
                    //     j.add("conditionType", conditionType);
                    //     j.add("currentUrl", getUrlSafely(driver));
                    //     j.add("timeout", getTimeoutFromWait(wait));
                    //     j.add("pollingInterval", getPollingIntervalFromWait(wait));
                    //     return null;
                    // });
                }
                
                return EventBus.bus().startStep("Wait.until", driverId(driver), waitType + "." + conditionType, null);
            } catch (Exception e) {
                System.out.println("[CDP Agent] WebDriverWait.until onEnter error: " + e.getMessage());
                e.printStackTrace();
                return 0L;
            }
        }

        @Advice.OnMethodExit(onThrowable = Throwable.class)
        static void onExit(@Advice.This Object wait, @Advice.Enter long start, @Advice.Return Object result, @Advice.Thrown Throwable t) {
            System.out.println("[CDP Agent] WebDriverWait.until interceptor exit");
            try {
                WebDriver driver = getDriverFromWait(wait);
                String waitType = wait.getClass().getSimpleName();
                long duration = System.currentTimeMillis() - start;
                
                // Emit the corresponding end event
                EventBus.bus().directEmit("Wait.until.end", wait, null);
                System.out.println("[CDP Agent] Wait.until.end event emitted");
                
                // Capture wait completion with timing and result details
                // EventBus.bus().cdp("Wait.until.completed", wait, j -> {
                //     j.add("waitType", waitType);
                //     j.add("success", t == null);
                //     j.add("duration", duration);
                //     j.add("resultFound", result != null);
                //     if (t != null) {
                //         j.add("error", t.getClass().getSimpleName());
                //         j.add("errorMessage", t.getMessage());
                //     }
                //     return null;
                // });
                
                EventBus.bus().endStep(start, "Wait.until", driverId(driver), waitType, t);
            } catch (Exception e) {
                System.out.println("[CDP Agent] WebDriverWait.until onExit error: " + e.getMessage());
            }
        }
        
        public static WebDriver getDriverFromWait(Object wait) {
            try {
                // Try to get driver from WebDriverWait or FluentWait
                java.lang.reflect.Field driverField = wait.getClass().getDeclaredField("driver");
                driverField.setAccessible(true);
                Object driver = driverField.get(wait);
                return driver instanceof WebDriver ? (WebDriver) driver : null;
            } catch (Exception e) {
                // Try alternative field names
                try {
                    java.lang.reflect.Field inputField = wait.getClass().getDeclaredField("input");
                    inputField.setAccessible(true);
                    Object input = inputField.get(wait);
                    return input instanceof WebDriver ? (WebDriver) input : null;
                } catch (Exception ex) {
                    System.out.println("[CDP Agent] Could not extract driver from wait object: " + ex.getMessage());
                    return null;
                }
            }
        }
        
        public static long getTimeoutFromWait(Object wait) {
            try {
                java.lang.reflect.Field timeoutField = wait.getClass().getDeclaredField("timeout");
                timeoutField.setAccessible(true);
                Object timeout = timeoutField.get(wait);
                if (timeout instanceof java.time.Duration) {
                    return ((java.time.Duration) timeout).toMillis();
                }
                return 0L;
            } catch (Exception e) {
                return 0L;
            }
        }
        
        public static long getPollingIntervalFromWait(Object wait) {
            try {
                java.lang.reflect.Field intervalField = wait.getClass().getDeclaredField("interval");
                intervalField.setAccessible(true);
                Object interval = intervalField.get(wait);
                if (interval instanceof java.time.Duration) {
                    return ((java.time.Duration) interval).toMillis();
                }
                return 0L;
            } catch (Exception e) {
                return 500L; // Default polling interval
            }
        }
    }

    // Implicit wait interceptor for WebDriver.manage().timeouts().implicitlyWait()
    public static class ImplicitWait {
        @Advice.OnMethodEnter
        static long onEnter(@Advice.This Object timeouts, @Advice.AllArguments Object[] args) {
            System.out.println("[CDP Agent] ImplicitWait interceptor triggered");
            System.out.println("[CDP Agent] EventBus.bus() is: " + EventBus.bus());
            try {
                System.out.println("[CDP Agent] Getting driver from timeouts object");
                WebDriver driver = getDriverFromTimeouts(timeouts);
                System.out.println("[CDP Agent] Driver obtained: " + (driver != null ? driver.getClass().getSimpleName() : "null"));
                long timeoutValue = getTimeoutValue(args);
                System.out.println("[CDP Agent] Timeout value: " + timeoutValue);
                
                if (EventBus.bus() != null) {
                    System.out.println("[CDP Agent] About to call EventBus.directEmit for ImplicitWait.set.start");
                    EventBus.bus().directEmit("ImplicitWait.set.start", timeouts, null);
                    System.out.println("[CDP Agent] ImplicitWait directEmit call completed");
                    
                    // Capture implicit wait configuration
                    // EventBus.bus().cdp("ImplicitWait.set", timeouts, j -> {
                    //     j.add("timeoutMs", timeoutValue);
                    //     j.add("currentUrl", getUrlSafely(driver));
                    //     return null;
                    // });
                }
                
                return EventBus.bus().startStep("ImplicitWait.set", driverId(driver), String.valueOf(timeoutValue), null);
            } catch (Exception e) {
                System.out.println("[CDP Agent] ImplicitWait onEnter error: " + e.getMessage());
                e.printStackTrace();
                return 0L;
            }
        }

        @Advice.OnMethodExit(onThrowable = Throwable.class)
        static void onExit(@Advice.This Object timeouts, @Advice.Enter long start, @Advice.Thrown Throwable t) {
            System.out.println("[CDP Agent] ImplicitWait interceptor exit");
            try {
                WebDriver driver = getDriverFromTimeouts(timeouts);
                
                // Emit the corresponding end event
                EventBus.bus().directEmit("ImplicitWait.set.end", timeouts, null);
                System.out.println("[CDP Agent] ImplicitWait.set.end event emitted");
                
                EventBus.bus().endStep(start, "ImplicitWait.set", driverId(driver), "timeout_configured", t);
            } catch (Exception e) {
                System.out.println("[CDP Agent] ImplicitWait onExit error: " + e.getMessage());
            }
        }
        
        public static WebDriver getDriverFromTimeouts(Object timeouts) {
            try {
                // Navigate from Timeouts -> Options -> WebDriver
                java.lang.reflect.Field optionsField = timeouts.getClass().getDeclaredField("this$0");
                optionsField.setAccessible(true);
                Object options = optionsField.get(timeouts);
                
                java.lang.reflect.Field driverField = options.getClass().getDeclaredField("this$0");
                driverField.setAccessible(true);
                Object driver = driverField.get(options);
                
                return driver instanceof WebDriver ? (WebDriver) driver : null;
            } catch (Exception e) {
                System.out.println("[CDP Agent] Could not extract driver from timeouts: " + e.getMessage());
                return null;
            }
        }
        
        public static long getTimeoutValue(Object[] args) {
            try {
                if (args.length > 0) {
                    Object timeout = args[0];
                    if (timeout instanceof java.time.Duration) {
                        return ((java.time.Duration) timeout).toMillis();
                    } else if (timeout instanceof Number) {
                        return ((Number) timeout).longValue();
                    }
                }
                return 0L;
            } catch (Exception e) {
                return 0L;
            }
        }
    }

    // Thread.sleep interceptor for capturing manual delays
    public static class ThreadSleep {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.AllArguments Object[] args) {
            System.out.println("[CDP Agent] Thread.sleep interceptor triggered");
            try {
                long sleepDuration = args.length > 0 && args[0] instanceof Number ? ((Number) args[0]).longValue() : 0L;
                
                if (EventBus.bus() != null) {
                    // Capture manual delay/sleep timing
                    // EventBus.bus().cdp("Thread.sleep", null, j -> {
                    //     j.add("sleepDurationMs", sleepDuration);
                    //     j.add("thread", Thread.currentThread().getName());
                    //     return null;
                    // });
                }
                
                return System.currentTimeMillis();
            } catch (Exception e) {
                System.out.println("[CDP Agent] Thread.sleep onEnter error: " + e.getMessage());
                return 0L;
            }
        }

        @Advice.OnMethodExit(suppress = Throwable.class, onThrowable = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.AllArguments Object[] args, @Advice.Thrown Throwable t) {
            System.out.println("[CDP Agent] Thread.sleep interceptor exit");
            try {
                long actualDuration = System.currentTimeMillis() - start;
                long expectedDuration = args.length > 0 && args[0] instanceof Number ? ((Number) args[0]).longValue() : 0L;
                
                // Capture sleep completion with actual vs expected timing
                // EventBus.bus().cdp("Thread.sleep.completed", null, j -> {
                //     j.add("expectedDurationMs", expectedDuration);
                //     j.add("actualDurationMs", actualDuration);
                //     j.add("success", t == null);
                //     j.add("thread", Thread.currentThread().getName());
                //     if (t != null) {
                //         j.add("error", t.getClass().getSimpleName());
                //         j.add("errorMessage", t.getMessage());
                //     }
                //     return null;
                // });
            } catch (Exception e) {
                System.out.println("[CDP Agent] Thread.sleep onExit error: " + e.getMessage());
            }
        }
    }

    // Assertion and validation interceptors for WebElement state checks
    public static class WebElementGetText {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This WebElement el) {
            System.out.println("[CDP Agent] WebElement.getText interceptor triggered on " + el.getClass().getName());
            
            // Capture element text retrieval start
            // EventBus.bus().cdp("WebElement.getText.start", el, j -> {
            //     j.add("action", "getText");
            //     j.add("elementLocator", el.toString());
            //     
            //     // Get comprehensive element information
            //     try {
            //         WebDriver driver = ((WrapsDriver) el).getWrappedDriver();
            //         ElementIdentifier.ElementInfo info = ElementIdentifier.getElementInfo(el, driver);
            //         
            //         j.add("tagName", info.tagName);
            //         j.add("id", info.id);
            //         j.add("class", info.className);
            //         j.add("cssSelector", info.cssSelector);
            //         j.add("xpath", info.xpath);
            //         j.add("isDisplayed", info.isDisplayed);
            //         j.add("isEnabled", info.isEnabled);
            //     } catch (Throwable ignored) {
            //         // Fallback to basic element data
            //         try {
            //             j.add("tagName", el.getTagName());
            //             j.add("id", el.getAttribute("id"));
            //             j.add("class", el.getAttribute("class"));
            //         } catch (Throwable ignored2) {}
            //     }
            //     return null;
            // });
            
            return EventBus.bus().startStep("getText", driverId(el), getCleanLocator(el), null);
        }
        
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This WebElement el, @Advice.Return String text, @Advice.Thrown Throwable t) {
            // Capture text retrieval completion with result
            // EventBus.bus().cdp("WebElement.getText.completed", el, j -> {
            //     j.add("action", "getText");
            //     j.add("success", t == null);
            //     j.add("retrievedText", text != null ? text : "");
            //     j.add("textLength", text != null ? text.length() : 0);
            //     j.add("duration", System.currentTimeMillis() - start);
            //     if (t != null) {
            //         j.add("error", t.getClass().getSimpleName());
            //         j.add("errorMessage", t.getMessage());
            //     }
            //     return null;
            // });
            
            EventBus.bus().endStep(start, "getText", driverId(el), getCleanLocator(el), t);
        }
    }

    public static class WebElementIsDisplayed {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This WebElement el) {
            System.out.println("[CDP Agent] WebElement.isDisplayed interceptor triggered on " + el.getClass().getName());
            
            // Capture element visibility check start
            EventBus.bus().cdp("WebElement.isDisplayed.start", el, j -> {
                j.add("action", "isDisplayed");
                j.add("elementLocator", getCleanLocator(el));
                
                // Get comprehensive element information
                try {
                    WebDriver driver = ((WrapsDriver) el).getWrappedDriver();
                    ElementIdentifier.ElementInfo info = ElementIdentifier.getElementInfo(el, driver);
                    
                    j.add("tagName", info.tagName);
                    j.add("id", info.id);
                    j.add("class", info.className);
                    j.add("cssSelector", info.cssSelector);
                    j.add("xpath", info.xpath);
                } catch (Throwable ignored) {
                    // Fallback to basic element data
                    try {
                        j.add("tagName", el.getTagName());
                        j.add("id", el.getAttribute("id"));
                        j.add("class", el.getAttribute("class"));
                    } catch (Throwable ignored2) {}
                }
                return null;
            });
            
            return EventBus.bus().startStep("isDisplayed", driverId(el), getCleanLocator(el), null);
        }
        
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This WebElement el, @Advice.Return boolean isDisplayed, @Advice.Thrown Throwable t) {
            // Capture visibility check completion with result
            EventBus.bus().cdp("WebElement.isDisplayed.completed", el, j -> {
                j.add("action", "isDisplayed");
                j.add("success", t == null);
                j.add("isDisplayed", isDisplayed);
                j.add("duration", System.currentTimeMillis() - start);
                if (t != null) {
                    j.add("error", t.getClass().getSimpleName());
                    j.add("errorMessage", t.getMessage());
                }
                return null;
            });
            
            EventBus.bus().endStep(start, "isDisplayed", driverId(el), getCleanLocator(el), t);
        }
    }

    public static class WebElementIsEnabled {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This WebElement el) {
            System.out.println("[CDP Agent] WebElement.isEnabled interceptor triggered on " + el.getClass().getName());
            
            // Capture element enabled state check start
            EventBus.bus().cdp("WebElement.isEnabled.start", el, j -> {
                j.add("action", "isEnabled");
                j.add("elementLocator", getCleanLocator(el));
                
                // Get comprehensive element information
                try {
                    WebDriver driver = ((WrapsDriver) el).getWrappedDriver();
                    ElementIdentifier.ElementInfo info = ElementIdentifier.getElementInfo(el, driver);
                    
                    j.add("tagName", info.tagName);
                    j.add("id", info.id);
                    j.add("class", info.className);
                    j.add("cssSelector", info.cssSelector);
                    j.add("xpath", info.xpath);
                    j.add("isDisplayed", info.isDisplayed);
                } catch (Throwable ignored) {
                    // Fallback to basic element data
                    try {
                        j.add("tagName", el.getTagName());
                        j.add("id", el.getAttribute("id"));
                        j.add("class", el.getAttribute("class"));
                    } catch (Throwable ignored2) {}
                }
                return null;
            });
            
            return EventBus.bus().startStep("isEnabled", driverId(el), getCleanLocator(el), null);
        }
        
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This WebElement el, @Advice.Return boolean isEnabled, @Advice.Thrown Throwable t) {
            // Capture enabled state check completion with result
            EventBus.bus().cdp("WebElement.isEnabled.completed", el, j -> {
                j.add("action", "isEnabled");
                j.add("success", t == null);
                j.add("isEnabled", isEnabled);
                j.add("duration", System.currentTimeMillis() - start);
                if (t != null) {
                    j.add("error", t.getClass().getSimpleName());
                    j.add("errorMessage", t.getMessage());
                }
                return null;
            });
            
            EventBus.bus().endStep(start, "isEnabled", driverId(el), getCleanLocator(el), t);
        }
    }

    public static class WebElementIsSelected {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This WebElement el) {
            System.out.println("[CDP Agent] WebElement.isSelected interceptor triggered on " + el.getClass().getName());
            
            // Capture element selection state check start
            EventBus.bus().cdp("WebElement.isSelected.start", el, j -> {
                j.add("action", "isSelected");
                j.add("elementLocator", getCleanLocator(el));
                
                // Get comprehensive element information
                try {
                    WebDriver driver = ((WrapsDriver) el).getWrappedDriver();
                    ElementIdentifier.ElementInfo info = ElementIdentifier.getElementInfo(el, driver);
                    
                    j.add("tagName", info.tagName);
                    j.add("id", info.id);
                    j.add("class", info.className);
                    j.add("type", info.type);
                    j.add("cssSelector", info.cssSelector);
                    j.add("xpath", info.xpath);
                    j.add("isDisplayed", info.isDisplayed);
                    j.add("isEnabled", info.isEnabled);
                } catch (Throwable ignored) {
                    // Fallback to basic element data
                    try {
                        j.add("tagName", el.getTagName());
                        j.add("id", el.getAttribute("id"));
                        j.add("class", el.getAttribute("class"));
                        j.add("type", el.getAttribute("type"));
                    } catch (Throwable ignored2) {}
                }
                return null;
            });
            
            return EventBus.bus().startStep("isSelected", driverId(el), getCleanLocator(el), null);
        }
        
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This WebElement el, @Advice.Return boolean isSelected, @Advice.Thrown Throwable t) {
            // Capture selection state check completion with result
            EventBus.bus().cdp("WebElement.isSelected.completed", el, j -> {
                j.add("action", "isSelected");
                j.add("success", t == null);
                j.add("isSelected", isSelected);
                j.add("duration", System.currentTimeMillis() - start);
                if (t != null) {
                    j.add("error", t.getClass().getSimpleName());
                    j.add("errorMessage", t.getMessage());
                }
                return null;
            });
            
            EventBus.bus().endStep(start, "isSelected", driverId(el), getCleanLocator(el), t);
        }
    }

    public static class WebElementGetAttribute {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This WebElement el, @Advice.AllArguments Object[] args) {
            System.out.println("[CDP Agent] WebElement.getAttribute interceptor triggered on " + el.getClass().getName());
            String attributeName = args.length > 0 ? args[0].toString() : "unknown";
            
            // Capture element attribute retrieval start
            EventBus.bus().cdp("WebElement.getAttribute.start", el, j -> {
                j.add("action", "getAttribute");
                j.add("attributeName", attributeName);
                j.add("elementLocator", getCleanLocator(el));
                
                // Get comprehensive element information
                try {
                    WebDriver driver = ((WrapsDriver) el).getWrappedDriver();
                    ElementIdentifier.ElementInfo info = ElementIdentifier.getElementInfo(el, driver);
                    
                    j.add("tagName", info.tagName);
                    j.add("id", info.id);
                    j.add("class", info.className);
                    j.add("cssSelector", info.cssSelector);
                    j.add("xpath", info.xpath);
                    j.add("isDisplayed", info.isDisplayed);
                    j.add("isEnabled", info.isEnabled);
                } catch (Throwable ignored) {
                    // Fallback to basic element data
                    try {
                        j.add("tagName", el.getTagName());
                        j.add("id", el.getAttribute("id"));
                        j.add("class", el.getAttribute("class"));
                    } catch (Throwable ignored2) {}
                }
                return null;
            });
            
            return EventBus.bus().startStep("getAttribute", driverId(el), getCleanLocator(el), attributeName);
        }
        
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This WebElement el, @Advice.AllArguments Object[] args, @Advice.Return String attributeValue, @Advice.Thrown Throwable t) {
            String attributeName = args.length > 0 ? args[0].toString() : "unknown";
            
            // Capture attribute retrieval completion with result
            EventBus.bus().cdp("WebElement.getAttribute.completed", el, j -> {
                j.add("action", "getAttribute");
                j.add("attributeName", attributeName);
                j.add("success", t == null);
                j.add("attributeValue", attributeValue != null ? attributeValue : "");
                j.add("hasValue", attributeValue != null && !attributeValue.isEmpty());
                j.add("duration", System.currentTimeMillis() - start);
                if (t != null) {
                    j.add("error", t.getClass().getSimpleName());
                    j.add("errorMessage", t.getMessage());
                }
                return null;
            });
            
            EventBus.bus().endStep(start, "getAttribute", driverId(el), getCleanLocator(el), t);
        }
    }
    
    // Enhanced WebElement interceptor with error handling and retry logic
    public static class WebElementEnhanced {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static long onEnter(@Advice.This WebElement el, @Advice.Origin("#m") String method, @Advice.AllArguments Object[] args) {
            System.out.println("[CDP Agent] Enhanced WebElement interceptor: " + method);
            
            // Log operation start with error handling context
            EventBus.bus().cdp("WebElement." + method + ".enhanced.start", el, j -> {
                j.add("action", method);
                j.add("elementLocator", getCleanLocator(el));
                j.add("retryEnabled", true);
                j.add("errorHandlingEnabled", true);
                
                // Add retry statistics
                try {
                    j.add("retryStats", ErrorHandler.getRetryStatisticsAsJson());
                } catch (Exception ignored) {}
                
                return null;
            });
            
            return EventBus.bus().startStep(method + ".enhanced", driverId(el), getCleanLocator(el), Arrays.toString(args));
        }
        
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter long start, @Advice.This WebElement el, @Advice.Origin("#m") String method, 
                          @Advice.Thrown Throwable t, @Advice.AllArguments Object[] args) {
            
            // Enhanced error logging with categorization
            if (t != null) {
                EventBus.bus().cdp("WebElement." + method + ".enhanced.error", el, j -> {
                    j.add("action", method);
                    j.add("errorType", t.getClass().getSimpleName());
                    j.add("errorMessage", t.getMessage());
                    j.add("elementLocator", getCleanLocator(el));
                    j.add("duration", System.currentTimeMillis() - start);
                    
                    // Add error categorization
                    String category = "UNKNOWN";
                    if (t instanceof NoSuchElementException) {
                        category = "ELEMENT_NOT_FOUND";
                    } else if (t instanceof StaleElementReferenceException) {
                        category = "STALE_ELEMENT";
                    } else if (t instanceof ElementNotInteractableException) {
                        category = "ELEMENT_NOT_INTERACTABLE";
                    } else if (t instanceof ElementClickInterceptedException) {
                        category = "CLICK_INTERCEPTED";
                    } else if (t instanceof TimeoutException) {
                        category = "TIMEOUT";
                    }
                    
                    j.add("errorCategory", category);
                    j.add("isRetryable", isRetryableError(t));
                    
                    return null;
                });
            } else {
                // Log successful operation
                EventBus.bus().cdp("WebElement." + method + ".enhanced.success", el, j -> {
                    j.add("action", method);
                    j.add("elementLocator", getCleanLocator(el));
                    j.add("duration", System.currentTimeMillis() - start);
                    return null;
                });
            }
            
            EventBus.bus().endStep(start, method + ".enhanced", driverId(el), getCleanLocator(el), t);
        }
        
        private static boolean isRetryableError(Throwable t) {
            return t instanceof StaleElementReferenceException ||
                   t instanceof ElementNotInteractableException ||
                   t instanceof ElementClickInterceptedException ||
                   t instanceof TimeoutException ||
                   t instanceof WebDriverException;
        }
    }
    
    // Error recovery interceptor for critical operations
    public static class WebElementWithRetry {
        @Advice.OnMethodEnter(suppress = Throwable.class)
        static Object[] onEnter(@Advice.This WebElement el, @Advice.Origin("#m") String method, @Advice.AllArguments Object[] args) {
            System.out.println("[CDP Agent] WebElement with retry: " + method);
            
            // Store original method call for potential retry
            return new Object[]{el, method, args, System.currentTimeMillis()};
        }
        
        @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
        static void onExit(@Advice.Enter Object[] context, @Advice.This WebElement el, @Advice.Origin("#m") String method,
                          @Advice.Thrown Throwable t, @Advice.Return Object result) {
            
            if (context == null || context.length < 4) return;
            
            long startTime = (Long) context[3];
            String operationId = method + "_" + getCleanLocator(el).hashCode();
            
            if (t != null) {
                // Log the error and potential retry information
                EventBus.bus().cdp("WebElement." + method + ".retry.error", el, j -> {
                    j.add("operationId", operationId);
                    j.add("action", method);
                    j.add("errorType", t.getClass().getSimpleName());
                    j.add("errorMessage", t.getMessage());
                    j.add("elementLocator", getCleanLocator(el));
                    j.add("duration", System.currentTimeMillis() - startTime);
                    j.add("retryRecommended", isRetryableError(t));
                    return null;
                });
            } else {
                // Log successful operation
                EventBus.bus().cdp("WebElement." + method + ".retry.success", el, j -> {
                    j.add("operationId", operationId);
                    j.add("action", method);
                    j.add("elementLocator", getCleanLocator(el));
                    j.add("duration", System.currentTimeMillis() - startTime);
                    j.add("result", result != null ? result.toString() : "void");
                    return null;
                });
            }
        }
        
        private static boolean isRetryableError(Throwable t) {
            return t instanceof StaleElementReferenceException ||
                   t instanceof ElementNotInteractableException ||
                   t instanceof ElementClickInterceptedException ||
                   t instanceof TimeoutException ||
                   t instanceof WebDriverException;
        }
    }
    
    /**
     * Generate a clean, usable locator string for WebElement instead of the wrapped representation
     */
    private static String getCleanLocator(WebElement element) {
        try {
            String elementString = element.toString();
            System.out.println("[CDP Agent] getCleanLocator called for element: " + elementString);
            
            // Try to get ID first
            String id = element.getAttribute("id");
            if (id != null && !id.trim().isEmpty()) {
                String result = "#" + id;
                System.out.println("[CDP Agent] getCleanLocator returning ID: " + result);
                return result;
            }
            
            // Try to get name attribute
            String name = element.getAttribute("name");
            if (name != null && !name.trim().isEmpty()) {
                String result = "[name='" + name + "']";
                System.out.println("[CDP Agent] getCleanLocator returning name: " + result);
                return result;
            }
            
            // Try to get class attribute
            String className = element.getAttribute("class");
            if (className != null && !className.trim().isEmpty()) {
                String result = "." + className.split("\\s+")[0]; // Use first class
                System.out.println("[CDP Agent] getCleanLocator returning class: " + result);
                return result;
            }
            
            // Try to get tag name
            String tagName = element.getTagName();
            if (tagName != null && !tagName.trim().isEmpty()) {
                System.out.println("[CDP Agent] getCleanLocator returning tagName: " + tagName);
                return tagName;
            }
            
            System.out.println("[CDP Agent] getCleanLocator returning unknown");
            return "unknown";
        } catch (Exception e) {
            System.out.println("[CDP Agent] getCleanLocator exception: " + e.getMessage());
            return "unknown";
        }
    }
}

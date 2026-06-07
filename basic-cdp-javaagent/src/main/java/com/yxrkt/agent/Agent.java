package com.yxrkt.agent;

import com.yxrkt.agent.bus.EventBus;
import com.yxrkt.agent.hooks.ChromeDriverHook;
import com.yxrkt.agent.hooks.SeleniumInterceptors;
import net.bytebuddy.agent.builder.AgentBuilder;
import net.bytebuddy.asm.Advice;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.dynamic.DynamicType;
import net.bytebuddy.asm.Advice;
import net.bytebuddy.matcher.ElementMatchers;

import java.lang.instrument.Instrumentation;

import static net.bytebuddy.matcher.ElementMatchers.*;

public class Agent {
    public static void premain(String args, Instrumentation inst) {
        Config.init(args);
        EventBus.init();

        System.out.println("[CDP Agent] Selenium CDP Agent loaded successfully");
        System.out.println("[CDP Agent] Configuration: " + args);
        System.out.println("[CDP Agent] Output path: " + Config.outPath());
        System.out.println("[CDP Agent] CDP enabled: " + Config.isCdpEnabled());
        
        // Initialize comprehensive WebDriver and WebElement interception using Advice
        try {
            // Global class discovery logging to debug ByteBuddy
            new AgentBuilder.Default()
                .with(AgentBuilder.RedefinitionStrategy.RETRANSFORMATION)
                .with(new AgentBuilder.Listener() {
                    @Override
                    public void onDiscovery(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        // Log all class discoveries to see what's being loaded
                        System.out.println("[CDP Agent] Class Discovery: " + typeName);
                    }
                    @Override
                    public void onTransformation(TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, DynamicType dynamicType) {
                        System.out.println("[CDP Agent] Class Transformed: " + typeDescription.getName());
                    }
                    @Override
                    public void onIgnored(TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        // Only log selenium-related ignored classes to reduce noise
                        if (typeDescription.getName().contains("selenium") || typeDescription.getName().contains("chrome")) {
                            System.out.println("[CDP Agent] Selenium Class Ignored: " + typeDescription.getName());
                        }
                    }
                    @Override
                    public void onError(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, Throwable throwable) {
                        System.out.println("[CDP Agent] Transformation Error: " + typeName + " - " + throwable.getMessage());
                    }
                    @Override
                    public void onComplete(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {}
                })
                .type(any()) // Match all classes to see what's happening
                .transform((builder, typeDescription, classLoader, module, protectionDomain) -> {
                    // Only transform ChromeDriver classes
                    if (typeDescription.getName().contains("ChromeDriver")) {
                        System.out.println("[CDP Agent] Transforming ChromeDriver: " + typeDescription.getName());
                        return builder
                        // Intercept navigation methods
                        .method(named("get"))
                        .intercept(Advice.to(SeleniumInterceptors.WebDriverGet.class))
                        
                        // Intercept element finding methods
                        .method(named("findElement").or(named("findElements")))
                        .intercept(Advice.to(SeleniumInterceptors.Finders.class));
                    }
                    return builder; // No transformation for other classes
                })
                .installOn(inst);
                
            // WebElement interception using hasSuperType approach like selenium-telemetry-agent
            new AgentBuilder.Default()
                .with(AgentBuilder.RedefinitionStrategy.RETRANSFORMATION)
                .with(new AgentBuilder.Listener() {
                     @Override
                     public void onDiscovery(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                         if (typeName.contains("selenium") || typeName.contains("WebElement")) {
                             System.out.println("[CDP Agent] WebElement Discovery: " + typeName + " (loaded: " + loaded + ")");
                         }
                     }
                     @Override
                     public void onTransformation(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, net.bytebuddy.dynamic.DynamicType dynamicType) {
                         System.out.println("[CDP Agent] WebElement Transformed: " + typeDescription.getName());
                     }
                     @Override
                     public void onIgnored(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                         if (typeDescription.getName().contains("selenium") || typeDescription.getName().contains("WebElement")) {
                             System.out.println("[CDP Agent] WebElement Ignored: " + typeDescription.getName() + " (loaded: " + loaded + ")");
                         }
                     }
                     @Override
                     public void onError(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, Throwable throwable) {
                         if (typeName.contains("selenium") || typeName.contains("WebElement")) {
                             System.out.println("[CDP Agent] WebElement Error: " + typeName + " - " + throwable.getMessage());
                         }
                     }
                     @Override
                     public void onComplete(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {}
                 })
                 .type(hasSuperType(named("org.openqa.selenium.WebElement")).and(not(isInterface())))
                .transform((builder, typeDescription, classLoader, module, protectionDomain) -> {
                    System.out.println("[CDP Agent] Transforming WebElement: " + typeDescription.getName());
                    return builder
                             .method(named("sendKeys"))
                             .intercept(Advice.to(SeleniumInterceptors.WebElementSendKeys.class))
                             .method(named("getText"))
                             .intercept(Advice.to(SeleniumInterceptors.WebElementGetText.class))
                             .method(named("isDisplayed"))
                             .intercept(Advice.to(SeleniumInterceptors.WebElementIsDisplayed.class))
                             .method(named("isEnabled"))
                             .intercept(Advice.to(SeleniumInterceptors.WebElementIsEnabled.class))
                             .method(named("isSelected"))
                             .intercept(Advice.to(SeleniumInterceptors.WebElementIsSelected.class))
                             .method(named("getAttribute"))
                             .intercept(Advice.to(SeleniumInterceptors.WebElementGetAttribute.class))
                             .method(named("click").or(named("clear")).or(named("submit"))
                                 .or(named("getTagName")).or(named("getCssValue")))
                             .intercept(Advice.to(SeleniumInterceptors.WebElementNoArgs.class));
                })
                .installOn(inst);
                
            // Check for already loaded Selenium classes and attempt retransformation
            try {
                Class<?>[] loadedClasses = inst.getAllLoadedClasses();
                System.out.println("[CDP Agent] Checking " + loadedClasses.length + " already loaded classes for Selenium types");
                
                for (Class<?> clazz : loadedClasses) {
                    String className = clazz.getName();
                    if (className.contains("selenium") || className.contains("WebElement") || className.contains("RemoteWebElement")) {
                        System.out.println("[CDP Agent] Found already loaded Selenium class: " + className);
                        if (inst.isRetransformClassesSupported() && inst.isModifiableClass(clazz)) {
                            System.out.println("[CDP Agent] Attempting retransformation of: " + className);
                            try {
                                inst.retransformClasses(clazz);
                                System.out.println("[CDP Agent] Successfully retransformed: " + className);
                            } catch (Exception retransEx) {
                                System.out.println("[CDP Agent] Failed to retransform " + className + ": " + retransEx.getMessage());
                            }
                        } else {
                            System.out.println("[CDP Agent] Class not modifiable: " + className);
                        }
                    }
                }
            } catch (Exception e) {
                System.out.println("[CDP Agent] Error checking loaded classes: " + e.getMessage());
            }
                
            // Navigation interception for RemoteNavigation
            new AgentBuilder.Default()
                .with(AgentBuilder.RedefinitionStrategy.RETRANSFORMATION)
                .with(new AgentBuilder.Listener() {
                    @Override
                    public void onDiscovery(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        if (typeName.contains("Navigation") || typeName.contains("Remote")) {
                            System.out.println("[CDP Agent] Navigation Discovery: " + typeName);
                        }
                    }
                    @Override
                    public void onTransformation(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, net.bytebuddy.dynamic.DynamicType dynamicType) {
                        System.out.println("[CDP Agent] Navigation Transformed: " + typeDescription.getName());
                    }
                    @Override
                    public void onIgnored(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        if (typeDescription.getName().contains("Navigation")) {
                            System.out.println("[CDP Agent] Navigation Ignored: " + typeDescription.getName());
                        }
                    }
                    @Override
                    public void onError(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, Throwable throwable) {
                        System.out.println("[CDP Agent] Navigation Error: " + typeName + " - " + throwable.getMessage());
                    }
                    @Override
                    public void onComplete(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {}
                })
                .type(named("org.openqa.selenium.remote.RemoteNavigation")
                    .or(nameEndsWith("Navigation").and(not(isInterface()))))
                .transform((builder, typeDescription, classLoader, module, protectionDomain) -> {
                    System.out.println("[CDP Agent] Transforming Navigation: " + typeDescription.getName());
                    return builder
                        .method(named("to").or(named("back")).or(named("forward")).or(named("refresh")))
                        .intercept(Advice.to(SeleniumInterceptors.WebDriverNavigation.class));
                })
                .installOn(inst);
                
            // Window/TargetLocator interception for RemoteTargetLocator
            new AgentBuilder.Default()
                .with(AgentBuilder.RedefinitionStrategy.RETRANSFORMATION)
                .with(new AgentBuilder.Listener() {
                    @Override
                    public void onDiscovery(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        if (typeName.contains("TargetLocator") || typeName.contains("Window")) {
                            System.out.println("[CDP Agent] Window Discovery: " + typeName);
                        }
                    }
                    @Override
                    public void onTransformation(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, net.bytebuddy.dynamic.DynamicType dynamicType) {
                        System.out.println("[CDP Agent] Window Transformed: " + typeDescription.getName());
                    }
                    @Override
                    public void onIgnored(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        if (typeDescription.getName().contains("TargetLocator") || typeDescription.getName().contains("Window")) {
                            System.out.println("[CDP Agent] Window Ignored: " + typeDescription.getName());
                        }
                    }
                    @Override
                    public void onError(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, Throwable throwable) {
                        System.out.println("[CDP Agent] Window Error: " + typeName + " - " + throwable.getMessage());
                    }
                    @Override
                    public void onComplete(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {}
                })
                .type(named("org.openqa.selenium.remote.RemoteTargetLocator")
                    .or(nameEndsWith("TargetLocator").and(not(isInterface()))))
                .transform((builder, typeDescription, classLoader, module, protectionDomain) -> {
                    System.out.println("[CDP Agent] Transforming TargetLocator: " + typeDescription.getName());
                    return builder
                        .method(named("window").or(named("frame")).or(named("defaultContent")).or(named("activeElement")))
                        .intercept(Advice.to(SeleniumInterceptors.WebDriverWindow.class));
                })
                .installOn(inst);
                
            // WebDriverWait interception for timing and synchronization data
            new AgentBuilder.Default()
                .with(AgentBuilder.RedefinitionStrategy.RETRANSFORMATION)
                .with(new AgentBuilder.Listener() {
                    @Override
                    public void onDiscovery(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        if (typeName.contains("WebDriverWait") || typeName.contains("FluentWait")) {
                            System.out.println("[CDP Agent] Wait Discovery: " + typeName);
                        }
                    }
                    @Override
                    public void onTransformation(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, net.bytebuddy.dynamic.DynamicType dynamicType) {
                        System.out.println("[CDP Agent] Wait Transformed: " + typeDescription.getName());
                    }
                    @Override
                    public void onIgnored(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        if (typeDescription.getName().contains("Wait")) {
                            System.out.println("[CDP Agent] Wait Ignored: " + typeDescription.getName());
                        }
                    }
                    @Override
                    public void onError(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, Throwable throwable) {
                        System.out.println("[CDP Agent] Wait Error: " + typeName + " - " + throwable.getMessage());
                    }
                    @Override
                    public void onComplete(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {}
                })
                .type(named("org.openqa.selenium.support.ui.WebDriverWait")
                    .or(named("org.openqa.selenium.support.ui.FluentWait"))
                    .or(nameEndsWith("Wait").and(not(isInterface()))))
                .transform((builder, typeDescription, classLoader, module, protectionDomain) -> {
                    System.out.println("[CDP Agent] Transforming Wait: " + typeDescription.getName());
                    return builder
                        .method(named("until"))
                        .intercept(Advice.to(SeleniumInterceptors.WebDriverWaitUntil.class));
                })
                .installOn(inst);
                
            // Thread.sleep interception for timing data
            new AgentBuilder.Default()
                .with(AgentBuilder.RedefinitionStrategy.RETRANSFORMATION)
                .with(new AgentBuilder.Listener() {
                    @Override
                    public void onDiscovery(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        if (typeName.equals("java.lang.Thread")) {
                            System.out.println("[CDP Agent] Thread Discovery: " + typeName);
                        }
                    }
                    @Override
                    public void onTransformation(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, net.bytebuddy.dynamic.DynamicType dynamicType) {
                        System.out.println("[CDP Agent] Thread Transformed: " + typeDescription.getName());
                    }
                    @Override
                    public void onIgnored(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        if (typeDescription.getName().equals("java.lang.Thread")) {
                            System.out.println("[CDP Agent] Thread Ignored: " + typeDescription.getName());
                        }
                    }
                    @Override
                    public void onError(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, Throwable throwable) {
                        System.out.println("[CDP Agent] Thread Error: " + typeName + " - " + throwable.getMessage());
                    }
                    @Override
                    public void onComplete(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {}
                })
                .type(named("java.lang.Thread"))
                .transform((builder, typeDescription, classLoader, module, protectionDomain) -> {
                    System.out.println("[CDP Agent] Transforming Thread: " + typeDescription.getName());
                    return builder
                        .method(named("sleep").and(isStatic()))
                        .intercept(Advice.to(SeleniumInterceptors.ThreadSleep.class));
                })
                .installOn(inst);
                
            // WebDriver implicit wait interception
            new AgentBuilder.Default()
                .with(AgentBuilder.RedefinitionStrategy.RETRANSFORMATION)
                .with(new AgentBuilder.Listener() {
                    @Override
                    public void onDiscovery(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        if (typeName.contains("Timeouts")) {
                            System.out.println("[CDP Agent] ImplicitWait Discovery: " + typeName);
                        }
                    }
                    @Override
                    public void onTransformation(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, net.bytebuddy.dynamic.DynamicType dynamicType) {
                        System.out.println("[CDP Agent] ImplicitWait Transformed: " + typeDescription.getName());
                    }
                    @Override
                    public void onIgnored(net.bytebuddy.description.type.TypeDescription typeDescription, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {
                        if (typeDescription.getName().contains("Timeouts")) {
                            System.out.println("[CDP Agent] ImplicitWait Ignored: " + typeDescription.getName());
                        }
                    }
                    @Override
                    public void onError(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded, Throwable throwable) {
                        System.out.println("[CDP Agent] ImplicitWait Error: " + typeName + " - " + throwable.getMessage());
                    }
                    @Override
                    public void onComplete(String typeName, ClassLoader classLoader, net.bytebuddy.utility.JavaModule module, boolean loaded) {}
                })
                .type(named("org.openqa.selenium.remote.RemoteWebDriver$RemoteWebDriverOptions$RemoteTimeouts")
                    .or(nameEndsWith("RemoteTimeouts").and(not(isInterface()))))
                .transform((builder, typeDescription, classLoader, module, protectionDomain) -> {
                    System.out.println("[CDP Agent] Transforming ImplicitWait: " + typeDescription.getName());
                    return builder
                        .method(named("implicitlyWait"))
                        .intercept(Advice.to(SeleniumInterceptors.ImplicitWait.class));
                })
                .installOn(inst);
                
            // Note: JavaScript execution and Actions API interception can be added later
            // Currently focusing on core WebDriver, WebElement, Navigation, Window, and Wait interactions
                
            System.out.println("[CDP Agent] WebDriver method interception installed successfully");
            System.out.println("[CDP Agent] Wait and timing interception installed successfully");
            
        } catch (Exception e) {
            System.err.println("[CDP Agent] Failed to install method interception: " + e.getMessage());
            e.printStackTrace();
        }
        
        System.out.println("[CDP Agent] Agent initialized with comprehensive interception");
        
        // Test EventBus output directly to verify it's working
        try {
            EventBus.bus().cdp("agent.test", null, j -> {
                j.add("message", "Agent test event");
                j.add("timestamp", System.currentTimeMillis());
                return null;
            });
            System.out.println("[CDP Agent] Test event written to output");
        } catch (Exception e) {
            System.out.println("[CDP Agent] Failed to write test event: " + e.getMessage());
        }
    }
}

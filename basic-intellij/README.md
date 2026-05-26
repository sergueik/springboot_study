# IntelliJ Plugin Policy (Git Externalized)

## 🚫 INTELLIJ MUST NOT HANDLE GIT / VCS UI

Disable or remove all VCS integrations:

- vcs-git
- vcs-github
- vcs-gitlab
- vcs-svn
- vcs-hg
- vcs-perforce

Also disable:
- Git tool window integrations
- Git UI annotations (blame, gutter markers)
- built-in commit / push / pull UI

---

## ✅ EXPECTED WORKFLOW

- Git operations are performed ONLY via CLI:
  - git status
  - git add
  - git commit
  - git rebase
  - git push
- IntelliJ is used only as:
  - code editor
  - debugger
  - build tool runner (Maven / Gradle)

---

## 🔧 INTELLIJ SETTINGS TO CHANGE

### Disable VCS UI completely:
Settings → Version Control:
- Remove all project VCS mappings
- Set "Version Control" = None

### Disable Git integration:
Settings → Version Control → Git:
- Uncheck:
  - "Use credential helper" (optional)
  - any auto-fetch / background refresh options

### Disable annotations:
- Turn off:
  - gutter blame annotations
  - code authorship hints
  - inline VCS indicators

---

## 💥 REMOVE THESE PLUGINS (SAFE FOR YOUR USE CASE)

Since Git is external:

- vcs-git
- vcs-github
- vcs-gitlab
- vcs-svn
- vcs-hg
- vcs-perforce

Optional cleanup:
- repository-search
- tasks (if tied to VCS workflow)

---

## ⚠️ CONSEQUENCE OF THIS SETUP

You lose:
- UI commit dialog
- inline diff viewer
- automatic change tracking UI

You keep:
- full manual Git control (CLI)
- zero IDE interference
- predictable state (no hidden indexing of VCS metadata)

---

## 🎯 DESIGN GOAL

IntelliJ becomes:
- deterministic editor
- no background VCS mutation
- no auto-scan or auto-fetch noise
- no repository state caching beyond file editing

### NOTE

one may either blunt
```sh
du -sh *
```
or 
```sh
ls -d1 * | xargs -IX sh -c 'export N=X; echo $N; du -sh '$(pwd)'/$N'
```

### Remove these
```sh
qodana
featuresTrainer
completionMlRanking
turboComplete
llmInstaller
performanceTesting
performanceTesting-async
emojipicker
repository-search
settingsSync
tasks
platform-ijent-impl
vcs-git
vcs-github
vcs-gitlab
vcs-svn
vcs-hg
vcs-perforce
android-gradle-dsl
gradle-analysis
gradle-dependencyUpdater
gradle-java-maven
Groovy
javaFX
textmate
uiDesigner
webp
platform-images
configurationScript
copyright
keymap-eclipse
keymap-netbeans
keymap-visualStudio
eclipse
html-tools
packageChecker
properties
searchEverywhereMl
dev
```

Optional, but probably recommended to keep:
```text
gradle
maven
java
Kotlin
terminal
yaml
markdown
sh
junit
testng
```


### Inventory

|size|name|
|----|----|
|3.0M|android-gradle-dsl|
|1.3M|completionMlRanking|
|356K|configurationScript|
|592K|copyright|
|97M|cwm-plugin|
|1.4M|dev|
|2.0M|eclipse|
|3.1M|editorconfig|
|11M|emojipicker|
|24M|featuresTrainer|
|160M|gradle|
|68K|gradle-analysis|
|48K|gradle-dependencyUpdater|
|3.0M|gradle-java|
|188K|gradle-java-maven|
|35M|grazie|
|21M|Groovy|
|384K|html-tools|
|2.3M|indexing-shared|
|113M|java|
|60K|java-byteCodeViewer|
|6.1M|java-coverage|
|1.3M|java-debugger-streams|
|1.9M|java-decompiler|
|1.4M|javaFX|
|784K|java-i18n|
|20K|java-ide-customization|
|1.7M|junit|
|40K|keymap-eclipse|
|28K|keymap-netbeans|
|56K|keymap-visualStudio|
|506M|Kotlin|
|724K|llmInstaller|
|4.9M|markdown|
|3.1M|marketplace|
|107M|maven|
|56K|maven-model|
|276K|maven-server|
|2.4M|packageChecker|
|21M|performanceTesting|
|23M|performanceTesting-async|
|72K|platform-ide-provisioner|
|15M|platform-ijent-impl|
|820K|platform-images|
|1.4M|platform-langInjection|
|60K|platform-tracing-ide|
|1.2M|properties|
|44M|qodana|
|1.4M|repository-search|
|4.5M|searchEverywhereMl|
|1.6M|settingsSync|
|1.4M|sh|
|7.4M|tasks|
|54M|terminal|
|4.0M|testng|
|9.5M|textmate|
|452K|toml|
|948K|turboComplete|
|3.4M|uiDesigner|
|13M|vcs-git|
|11M|vcs-github|
|12M|vcs-gitlab|
|1.7M|vcs-hg|
|2.4M|vcs-perforce|
|28M|vcs-svn|
|776K|webp|
|1.6M|yaml|

that was a nice try. it only freed approx 300M but made idea unable to load prohect:
```
intellij.lang.properties.xml.XmlPropertiesIndex) [Plugin: com.intellij.properties]
	at com.intellij.serviceContainer.ComponentManagerImpl.createError(ComponentManagerImpl.kt:988)
	at com.intellij.openapi.extensions.impl.XmlExtensionAdapter.doCreateInstance(XmlExtensionAdapter.kt:73)
	at com.intellij.openapi.extensions.impl.XmlExtensionAdapter.createInstance(XmlExtensionAdapter.kt:33)
	at com.intellij.openapi.extensions.impl.ExtensionPointImplKt.getOrCreateExtensionInstance(ExtensionPointImpl.kt:1052)
	at com.intellij.openapi.extensions.impl.ExtensionPointImplKt.access$getOrCreateExtensionInstance(ExtensionPointImpl.kt:1)
	at com.intellij.openapi.extensions.impl.ExtensionPointImpl$iterator$2.advanceToNextNonNull(ExtensionPointImpl.kt:283)
	at com.intellij.openapi.extensions.impl.ExtensionPointImpl$iterator$2.hasNext(ExtensionPointImpl.kt:265)
	at com.intellij.util.indexing.FileBasedIndexDataInitialization.initAssociatedDataForExtensions(FileBasedIndexDataInitialization.java:72)
	at com.intellij.util.indexing.FileBasedIndexDataInitialization.prepareTasks(FileBasedIndexDataInitialization.java:144)
	at com.intellij.util.indexing.IndexDataInitializer.call(IndexDataInitializer.kt:22)
	at com.intellij.util.indexing.IndexDataInitializer$Companion$submitGenesisTask$1.invokeSuspend(IndexDataInitializer.kt:61)
	at kotlin.coroutines.jvm.internal.BaseContinuationImpl.resumeWith(ContinuationImpl.kt:33)
	at kotlinx.coroutines.DispatchedTask.run(DispatchedTask.kt:108)
	at kotlinx.coroutines.internal.LimitedDispatcher$Worker.run(LimitedDispatcher.kt:115)
	at kotlinx.coroutines.scheduling.TaskImpl.run(Tasks.kt:103)
	at kotlinx.coroutines.scheduling.CoroutineScheduler.runSafely(CoroutineScheduler.kt:584)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.executeTask(CoroutineScheduler.kt:793)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.runWorker(CoroutineScheduler.kt:697)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.run(CoroutineScheduler.kt:684)
Caused by: java.lang.ClassNotFoundException: com.intellij.lang.properties.xml.XmlPropertiesIndex PluginClassLoader(plugin=PluginDescriptor(name=Properties, id=com.intellij.properties, descriptorPath=plugin.xml, path=~/idea-IC-241.19416.15/plugins/properties, version=241.19416.15, package=com.intellij.lang.properties, isBundled=true), packagePrefix=com.intellij.lang.properties., state=active)
	at com.intellij.serviceContainer.ComponentManagerImplKt.doLoadClass(ComponentManagerImpl.kt:1466)
	at com.intellij.serviceContainer.ComponentManagerImpl.loadClass(ComponentManagerImpl.kt:895)
	at com.intellij.openapi.extensions.impl.InterfaceExtensionImplementationClassResolver.resolveImplementationClass(InterfaceExtensionImplementationClassResolver.kt:20)
	at com.intellij.openapi.extensions.impl.XmlExtensionAdapter.doCreateInstance(XmlExtensionAdapter.kt:51)
	... 17 more
2026-05-26 09:24:00,007 [   5333] SEVERE - #c.i.o.e.i.ExtensionPointImpl - IntelliJ IDEA 2024.1.7  Build #IC-241.19416.15
2026-05-26 09:24:00,011 [   5337] SEVERE - #c.i.o.e.i.ExtensionPointImpl - JDK: 17.0.12; VM: OpenJDK 64-Bit Server VM; Vendor: JetBrains s.r.o.
2026-05-26 09:24:00,011 [   5337] SEVERE - #c.i.o.e.i.ExtensionPointImpl - OS: Linux
2026-05-26 09:24:00,013 [   5339] SEVERE - #c.i.o.e.i.ExtensionPointImpl - Last Action: 
2026-05-26 09:24:00,323 [   5649]   WARN - #c.i.i.s.p.i.BundledSharedIndexProvider - Bundled shared index is not found at: /home/sergueik/idea-IC-241.19416.15/jdk-shared-indexes
2026-05-26 09:24:00,855 [   6181] SEVERE - #c.i.o.e.i.ExtensionPointImpl - Cannot create extension (class=org.jetbrains.idea.reposearch.statistics.TopPackageIdValidationRule) [Plugin: org.jetbrains.idea.reposearch]
com.intellij.diagnostic.PluginException: Cannot create extension (class=org.jetbrains.idea.reposearch.statistics.TopPackageIdValidationRule) [Plugin: org.jetbrains.idea.reposearch]
	at com.intellij.serviceContainer.ComponentManagerImpl.createError(ComponentManagerImpl.kt:988)
	at com.intellij.openapi.extensions.impl.XmlExtensionAdapter.doCreateInstance(XmlExtensionAdapter.kt:73)
	at com.intellij.openapi.extensions.impl.XmlExtensionAdapter.createInstance(XmlExtensionAdapter.kt:33)
	at com.intellij.openapi.extensions.impl.ExtensionPointImpl.processAdapter(ExtensionPointImpl.kt:395)
	at com.intellij.openapi.extensions.impl.ExtensionPointImpl.createExtensionInstances(ExtensionPointImpl.kt:368)
	at com.intellij.openapi.extensions.impl.ExtensionPointImpl.getExtensionList(ExtensionPointImpl.kt:214)
	at com.intellij.openapi.extensions.ExtensionPointName.getExtensionList(ExtensionPointName.kt:54)
	at com.intellij.internal.statistic.eventLog.validator.rules.utils.CustomRuleProducer.createValidationRule(CustomRuleProducer.java:26)
	at com.intellij.internal.statistic.eventLog.validator.rules.utils.CustomRuleProducer.createValidationRule(CustomRuleProducer.java:16)
	at com.intellij.internal.statistic.eventLog.validator.rules.utils.ValidationSimpleRuleFactory.createSimpleRule(ValidationSimpleRuleFactory.java:82)
	at com.intellij.internal.statistic.eventLog.validator.rules.utils.ValidationSimpleRuleFactory.createExpressionRule(ValidationSimpleRuleFactory.java:95)
	at com.intellij.internal.statistic.eventLog.validator.rules.utils.ValidationSimpleRuleFactory.createRule(ValidationSimpleRuleFactory.java:71)
	at com.intellij.internal.statistic.eventLog.validator.rules.utils.ValidationSimpleRuleFactory.getRules(ValidationSimpleRuleFactory.java:44)
	at com.intellij.internal.statistic.eventLog.validator.rules.beans.EventGroupRules.<init>(EventGroupRules.java:53)
	at com.intellij.internal.statistic.eventLog.validator.rules.beans.EventGroupRules.create(EventGroupRules.java:214)
	at com.intellij.internal.statistic.eventLog.validator.storage.ValidationRulesPersistedStorage.lambda$createValidators$2(ValidationRulesPersistedStorage.java:154)
	at java.base/java.util.stream.Collectors.lambda$uniqKeysMapAccumulator$1(Collectors.java:180)
	at java.base/java.util.stream.ReduceOps$3ReducingSink.accept(ReduceOps.java:169)
	at java.base/java.util.stream.ReferencePipeline$2$1.accept(ReferencePipeline.java:179)
	at java.base/java.util.ArrayList$ArrayListSpliterator.forEachRemaining(ArrayList.java:1625)
	at java.base/java.util.stream.AbstractPipeline.copyInto(AbstractPipeline.java:509)
	at java.base/java.util.stream.AbstractPipeline.wrapAndCopyInto(AbstractPipeline.java:499)
	at java.base/java.util.stream.ReduceOps$ReduceOp.evaluateSequential(ReduceOps.java:921)
	at java.base/java.util.stream.AbstractPipeline.evaluate(AbstractPipeline.java:234)
	at java.base/java.util.stream.ReferencePipeline.collect(ReferencePipeline.java:682)
	at com.intellij.internal.statistic.eventLog.validator.storage.ValidationRulesPersistedStorage.createValidators(ValidationRulesPersistedStorage.java:153)
	at com.intellij.internal.statistic.eventLog.validator.storage.ValidationRulesPersistedStorage.createValidators(ValidationRulesPersistedStorage.java:142)
	at com.intellij.internal.statistic.eventLog.validator.storage.ValidationRulesPersistedStorage.updateValidators(ValidationRulesPersistedStorage.java:83)
	at com.intellij.internal.statistic.eventLog.validator.storage.ValidationRulesPersistedStorage.loadValidatorsFromLocalCache(ValidationRulesPersistedStorage.java:67)
	at com.intellij.internal.statistic.eventLog.validator.storage.ValidationRulesPersistedStorage.<init>(ValidationRulesPersistedStorage.java:43)
	at com.intellij.internal.statistic.eventLog.validator.storage.ValidationRulesStorageProvider.newStorage(ValidationRulesStorageProvider.java:11)
	at com.intellij.internal.statistic.eventLog.validator.IntellijSensitiveDataValidator.lambda$getInstance$0(IntellijSensitiveDataValidator.java:111)
	at java.base/java.util.concurrent.ConcurrentHashMap.computeIfAbsent(ConcurrentHashMap.java:1708)
	at com.intellij.internal.statistic.eventLog.validator.IntellijSensitiveDataValidator.getInstance(IntellijSensitiveDataValidator.java:108)
	at com.intellij.internal.statistic.eventLog.LocalStatisticsFileEventLogger.logAsync$lambda$0(LocalStatisticsFileEventLogger.kt:39)
	at java.base/java.util.concurrent.CompletableFuture$AsyncRun.run(CompletableFuture.java:1804)
	at com.intellij.util.concurrency.BoundedTaskExecutor.doRun(BoundedTaskExecutor.java:244)
	at com.intellij.util.concurrency.BoundedTaskExecutor.access$200(BoundedTaskExecutor.java:30)
	at com.intellij.util.concurrency.BoundedTaskExecutor$1.executeFirstTaskAndHelpQueue(BoundedTaskExecutor.java:222)
	at com.intellij.util.ConcurrencyUtil.runUnderThreadName(ConcurrencyUtil.java:218)
	at com.intellij.util.concurrency.BoundedTaskExecutor$1.run(BoundedTaskExecutor.java:210)
	at java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1136)
	at java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:635)
	at java.base/java.util.concurrent.Executors$PrivilegedThreadFactory$1$1.run(Executors.java:702)
	at java.base/java.util.concurrent.Executors$PrivilegedThreadFactory$1$1.run(Executors.java:699)
	at java.base/java.security.AccessController.doPrivileged(AccessController.java:399)
	at java.base/java.util.concurrent.Executors$PrivilegedThreadFactory$1.run(Executors.java:699)
	at java.base/java.lang.Thread.run(Thread.java:840)
Caused by: java.lang.ClassNotFoundException: org.jetbrains.idea.reposearch.statistics.TopPackageIdValidationRule PluginClassLoader(plugin=PluginDescriptor(name=JetBrains Repository Search, id=org.jetbrains.idea.reposearch, descriptorPath=plugin.xml, path=~/idea-IC-241.19416.15/plugins/repository-search, version=241.19416.15, package=null, isBundled=true), packagePrefix=null, state=active)
	at com.intellij.serviceContainer.ComponentManagerImplKt.doLoadClass(ComponentManagerImpl.kt:1466)
	at com.intellij.serviceContainer.ComponentManagerImpl.loadClass(ComponentManagerImpl.kt:895)
	at com.intellij.openapi.extensions.impl.InterfaceExtensionImplementationClassResolver.resolveImplementationClass(InterfaceExtensionImplementationClassResolver.kt:20)
	at com.intellij.openapi.extensions.impl.XmlExtensionAdapter.doCreateInstance(XmlExtensionAdapter.kt:51)
	... 46 more
2026-05-26 09:24:00,860 [   6186] SEVERE - #c.i.o.e.i.ExtensionPointImpl - IntelliJ IDEA 2024.1.7  Build #IC-241.19416.15
2026-05-26 09:24:00,863 [   6189] SEVERE - #c.i.o.e.i.ExtensionPointImpl - JDK: 17.0.12; VM: OpenJDK 64-Bit Server VM; Vendor: JetBrains s.r.o.
2026-05-26 09:24:00,864 [   6190] SEVERE - #c.i.o.e.i.ExtensionPointImpl - OS: Linux
2026-05-26 09:24:00,864 [   6190] SEVERE - #c.i.o.e.i.ExtensionPointImpl - Last Action: 
2026-05-26 09:24:01,228 [   6554] SEVERE - #c.i.o.e.i.ExtensionPointImpl - Cannot create extension (class=com.intellij.lang.properties.psi.PropertyKeyIndex) [Plugin: com.intellij.properties]
com.intellij.diagnostic.PluginException: Cannot create extension (class=com.intellij.lang.properties.psi.PropertyKeyIndex) [Plugin: com.intellij.properties]
	at com.intellij.serviceContainer.ComponentManagerImpl.createError(ComponentManagerImpl.kt:988)
	at com.intellij.openapi.extensions.impl.XmlExtensionAdapter.doCreateInstance(XmlExtensionAdapter.kt:73)
	at com.intellij.openapi.extensions.impl.XmlExtensionAdapter.createInstance(XmlExtensionAdapter.kt:33)
	at com.intellij.openapi.extensions.impl.ExtensionPointImplKt.getOrCreateExtensionInstance(ExtensionPointImpl.kt:1052)
	at com.intellij.openapi.extensions.impl.ExtensionPointImplKt.access$getOrCreateExtensionInstance(ExtensionPointImpl.kt:1)
	at com.intellij.openapi.extensions.impl.ExtensionPointImpl$iterator$2.advanceToNextNonNull(ExtensionPointImpl.kt:283)
	at com.intellij.openapi.extensions.impl.ExtensionPointImpl$iterator$2.hasNext(ExtensionPointImpl.kt:265)
	at com.intellij.psi.stubs.StubIndexImpl$StubIndexInitialization.prepareTasks(StubIndexImpl.java:393)
	at com.intellij.util.indexing.IndexDataInitializer.call(IndexDataInitializer.kt:22)
	at com.intellij.util.indexing.IndexDataInitializer$Companion$submitGenesisTask$1.invokeSuspend(IndexDataInitializer.kt:61)
	at kotlin.coroutines.jvm.internal.BaseContinuationImpl.resumeWith(ContinuationImpl.kt:33)
	at kotlinx.coroutines.DispatchedTask.run(DispatchedTask.kt:108)
	at kotlinx.coroutines.internal.LimitedDispatcher$Worker.run(LimitedDispatcher.kt:115)
	at kotlinx.coroutines.scheduling.TaskImpl.run(Tasks.kt:103)
	at kotlinx.coroutines.scheduling.CoroutineScheduler.runSafely(CoroutineScheduler.kt:584)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.executeTask(CoroutineScheduler.kt:793)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.runWorker(CoroutineScheduler.kt:697)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.run(CoroutineScheduler.kt:684)
Caused by: java.lang.ClassNotFoundException: com.intellij.lang.properties.psi.PropertyKeyIndex PluginClassLoader(plugin=PluginDescriptor(name=Properties, id=com.intellij.properties, descriptorPath=plugin.xml, path=~/idea-IC-241.19416.15/plugins/properties, version=241.19416.15, package=com.intellij.lang.properties, isBundled=true), packagePrefix=com.intellij.lang.properties., state=active)
	at com.intellij.serviceContainer.ComponentManagerImplKt.doLoadClass(ComponentManagerImpl.kt:1466)
	at com.intellij.serviceContainer.ComponentManagerImpl.loadClass(ComponentManagerImpl.kt:895)
	at com.intellij.openapi.extensions.impl.InterfaceExtensionImplementationClassResolver.resolveImplementationClass(InterfaceExtensionImplementationClassResolver.kt:20)
	at com.intellij.openapi.extensions.impl.XmlExtensionAdapter.doCreateInstance(XmlExtensionAdapter.kt:51)
	... 16 more
2026-05-26 09:24:01,232 [   6558] SEVERE - #c.i.o.e.i.ExtensionPointImpl - IntelliJ IDEA 2024.1.7  Build #IC-241.19416.15
2026-05-26 09:24:01,232 [   6558] SEVERE - #c.i.o.e.i.ExtensionPointImpl - JDK: 17.0.12; VM: OpenJDK 64-Bit Server VM; Vendor: JetBrains s.r.o.
2026-05-26 09:24:01,233 [   6559] SEVERE - #c.i.o.e.i.ExtensionPointImpl - OS: Linux
2026-05-26 09:24:01,238 [   6564] SEVERE - #c.i.o.e.i.ExtensionPointImpl - Last Action: 
2026-05-26 09:24:04,748 [  10074] SEVERE - #c.i.o.p.i.ProjectManagerImpl - project loading failed
java.lang.ClassNotFoundException: com.intellij.configurationScript.providers.ConfigurationScriptProjectStoreFactory PluginClassLoader(plugin=PluginDescriptor(name=Configuration Script, id=com.intellij.configurationScript, descriptorPath=plugin.xml, path=~/idea-IC-241.19416.15/plugins/configurationScript, version=241.19416.15, package=com.intellij.configurationScript, isBundled=true), packagePrefix=com.intellij.configurationScript., state=active)
	at com.intellij.serviceContainer.ComponentManagerImplKt.doLoadClass(ComponentManagerImpl.kt:1466)
	at com.intellij.serviceContainer.ServiceDescriptorInstanceInitializer.loadInstanceClass(ServiceInstanceInitializer.kt:91)
	at com.intellij.platform.instanceContainer.internal.LazyInstanceHolder.tryInitialize(LazyInstanceHolder.kt:117)
	at com.intellij.platform.instanceContainer.internal.LazyInstanceHolder.getInstance(LazyInstanceHolder.kt:95)
	at com.intellij.platform.instanceContainer.internal.LazyInstanceHolder.getInstanceInCallerContext$suspendImpl(LazyInstanceHolder.kt:87)
	at com.intellij.platform.instanceContainer.internal.LazyInstanceHolder.getInstanceInCallerContext(LazyInstanceHolder.kt)
	at com.intellij.serviceContainer.ComponentManagerImplKt$getOrCreateInstanceBlocking$3.invokeSuspend(ComponentManagerImpl.kt:1548)
	at kotlin.coroutines.jvm.internal.BaseContinuationImpl.resumeWith(ContinuationImpl.kt:33)
	at kotlinx.coroutines.DispatchedTask.run(DispatchedTask.kt:108)
	at kotlinx.coroutines.EventLoopImplBase.processNextEvent(EventLoop.common.kt:280)
	at kotlinx.coroutines.BlockingCoroutine.joinBlocking(Builders.kt:85)
	at kotlinx.coroutines.BuildersKt__BuildersKt.runBlocking(Builders.kt:59)
	at kotlinx.coroutines.BuildersKt.runBlocking(Unknown Source)
	at com.intellij.serviceContainer.ComponentManagerImplKt$runBlockingInitialization$1.invoke(ComponentManagerImpl.kt:1636)
	at com.intellij.serviceContainer.ComponentManagerImplKt$runBlockingInitialization$1.invoke(ComponentManagerImpl.kt:1627)
	at com.intellij.openapi.progress.ContextKt.prepareThreadContext(context.kt:83)
	at com.intellij.serviceContainer.ComponentManagerImplKt.runBlockingInitialization(ComponentManagerImpl.kt:1627)
	at com.intellij.serviceContainer.ComponentManagerImplKt.getOrCreateInstanceBlocking(ComponentManagerImpl.kt:1547)
	at com.intellij.serviceContainer.ComponentManagerImpl.doGetService(ComponentManagerImpl.kt:754)
	at com.intellij.serviceContainer.ComponentManagerImpl.getService(ComponentManagerImpl.kt:698)
	at com.intellij.openapi.project.impl.ProjectImpl$componentStoreValue$1.invoke(ProjectImpl.kt:418)
	at com.intellij.openapi.project.impl.ProjectImpl$componentStoreValue$1.invoke(ProjectImpl.kt:126)
	at com.intellij.util.concurrency.SynchronizedClearableLazy._get_value_$lambda$1$lambda$0(SynchronizedClearableLazy.kt:41)
	at java.base/java.util.concurrent.atomic.AtomicReference.updateAndGet(AtomicReference.java:210)
	at com.intellij.util.concurrency.SynchronizedClearableLazy.getValue(SynchronizedClearableLazy.kt:40)
	at com.intellij.openapi.project.impl.ProjectImpl.getComponentStore(ProjectImpl.kt:217)
	at com.intellij.openapi.project.impl.ProjectManagerImplKt.initProject(ProjectManagerImpl.kt:1215)
	at com.intellij.openapi.project.impl.ProjectManagerImplKt.access$initProject(ProjectManagerImpl.kt:1)
	at com.intellij.openapi.project.impl.ProjectManagerImpl.prepareProject(ProjectManagerImpl.kt:892)
	at com.intellij.openapi.project.impl.ProjectManagerImpl.access$prepareProject(ProjectManagerImpl.kt:98)
	at com.intellij.openapi.project.impl.ProjectManagerImpl$doOpenAsync$2.invokeSuspend(ProjectManagerImpl.kt:649)
	at com.intellij.openapi.project.impl.ProjectManagerImpl$doOpenAsync$2.invoke(ProjectManagerImpl.kt)
	at com.intellij.openapi.project.impl.ProjectManagerImpl$doOpenAsync$2.invoke(ProjectManagerImpl.kt)
	at com.intellij.openapi.project.impl.ProjectUiFrameAllocator$doRun$2.invokeSuspend(ProjectFrameAllocator.kt:228)
	at com.intellij.openapi.project.impl.ProjectUiFrameAllocator$doRun$2.invoke(ProjectFrameAllocator.kt)
	at com.intellij.openapi.project.impl.ProjectUiFrameAllocator$doRun$2.invoke(ProjectFrameAllocator.kt)
	at kotlinx.coroutines.intrinsics.UndispatchedKt.startUndispatchedOrReturn(Undispatched.kt:78)
	at kotlinx.coroutines.CoroutineScopeKt.coroutineScope(CoroutineScope.kt:264)
	at com.intellij.openapi.project.impl.ProjectUiFrameAllocator.doRun(ProjectFrameAllocator.kt:173)
	at com.intellij.openapi.project.impl.ProjectUiFrameAllocator.access$doRun(ProjectFrameAllocator.kt:150)
	at com.intellij.openapi.project.impl.ProjectUiFrameAllocator$run$2.invokeSuspend(ProjectFrameAllocator.kt:162)
	at com.intellij.openapi.project.impl.ProjectUiFrameAllocator$run$2.invoke(ProjectFrameAllocator.kt)
	at com.intellij.openapi.project.impl.ProjectUiFrameAllocator$run$2.invoke(ProjectFrameAllocator.kt)
	at kotlinx.coroutines.intrinsics.UndispatchedKt.startUndispatchedOrReturn(Undispatched.kt:78)
	at kotlinx.coroutines.CoroutineScopeKt.coroutineScope(CoroutineScope.kt:264)
	at com.intellij.openapi.project.impl.ProjectUiFrameAllocator.run(ProjectFrameAllocator.kt:155)
	at com.intellij.openapi.project.impl.ProjectManagerImpl.doOpenAsync(ProjectManagerImpl.kt:642)
	at com.intellij.openapi.project.impl.ProjectManagerImpl.access$doOpenAsync(ProjectManagerImpl.kt:98)
	at com.intellij.openapi.project.impl.ProjectManagerImpl$openProjectAsync$4.invokeSuspend(ProjectManagerImpl.kt:625)
	at com.intellij.openapi.project.impl.ProjectManagerImpl$openProjectAsync$4.invoke(ProjectManagerImpl.kt)
	at com.intellij.openapi.project.impl.ProjectManagerImpl$openProjectAsync$4.invoke(ProjectManagerImpl.kt)
	at kotlinx.coroutines.intrinsics.UndispatchedKt.startUndispatchedOrReturn(Undispatched.kt:78)
	at kotlinx.coroutines.BuildersKt__Builders_commonKt.withContext(Builders.common.kt:167)
	at kotlinx.coroutines.BuildersKt.withContext(Unknown Source)
	at com.intellij.platform.diagnostic.telemetry.impl.TracerKt.span(tracer.kt:53)
	at com.intellij.platform.diagnostic.telemetry.impl.TracerKt.span$default(tracer.kt:49)
	at com.intellij.openapi.project.impl.ProjectManagerImpl.openProjectAsync(ProjectManagerImpl.kt:624)
	at com.intellij.ide.RecentProjectsManagerBase.openProject$suspendImpl(RecentProjectsManagerBase.kt:308)
	at com.intellij.ide.RecentProjectsManagerBase$openProject$1.invokeSuspend(RecentProjectsManagerBase.kt)
	at kotlin.coroutines.jvm.internal.BaseContinuationImpl.resumeWith(ContinuationImpl.kt:33)
	at kotlinx.coroutines.DispatchedTask.run(DispatchedTask.kt:108)
	at kotlinx.coroutines.scheduling.CoroutineScheduler.runSafely(CoroutineScheduler.kt:584)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.executeTask(CoroutineScheduler.kt:793)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.runWorker(CoroutineScheduler.kt:697)
	at kotlinx.coroutines.scheduling.CoroutineScheduler$Worker.run(CoroutineScheduler.kt:684)
2026-05-26 09:24:04,756 [  10082] SEVERE - #c.i.o.p.i.ProjectManagerImpl - IntelliJ IDEA 2024.1.7  Build #IC-241.19416.15
2026-05-26 09:24:04,757 [  10083] SEVERE - #c.i.o.p.i.ProjectManagerImpl - JDK: 17.0.12; VM: OpenJDK 64-Bit Server VM; Vendor: JetBrains s.r.o.
2026-05-26 09:24:04,761 [  10087] SEVERE - #c.i.o.p.i.ProjectManagerImpl - OS: Linux
2026-05-26 09:24:04,762 [  10088] SEVERE - #c.i.o.p.i.ProjectManagerImpl - Last Action: 
```

### Recovery

Restore these directories from original archive or reinstall IDEA over itself:
```
properties
configurationScript
repository-search
```

Those three are __required__ despite looking optional.


### Install
on the host:
```sh
scp ~/Downloads/ideaIC-2024.1.7.tar.gz sergueik@192.168.12.161:Downloads
```
on the VM:
```sh
mkisofs -o /tmp/cd.iso .
sudo apt-get install genisoimage
pushd ~/Downloads
mkisofs -o /tmp/cd.iso .
sudo apt-get remove genisoimage
rm -fr ~/idea-IC-241.19416.15/
mv /tmp/cd.iso  ~/Downloads
```
on the host:
```sh
scp sergueik@192.168.12.161:Downloads/cd.iso .
```
on the VM:
 
```
cd ~; tar xzvf /media/sergueik/CDROM/ideaic_2.tgz 
```

```sh
FILES="qodana featuresTrainer completionMlRanking turboComplete llmInstaller performanceTesting performanceTesting-async emojipicker vcs-svn vcs-hg vcs-perforce android-gradle-dsl gradle-analysis gradle-dependencyUpdater gradle-java-maven javaFX textmate uiDesigner webp platform-images copyright keymap-eclipse keymap-netbeans keymap-visualStudio eclipse dev"

for F in $FILES; do
  echo rm -rf "/home/sergueik/idea-IC-241.19416.15/plugins/$F"
done
```

### Info

this folder contains replica of `skills` directory of the
[UiPath Agent Skills](https://github.com/UiPath/skills) repository
an self-contained AI agent skills for UiPath automation development.

### Usage

refresh
```powershell
$URL='https://github.com/UiPath/skills/tree/main/skills'
./restore.cmd $URL "$env:TEMP" 
pushd $env:TEMP
# the restore.ps1 does not appear to honor the destination dir
popd
copy-item -literalpath skills -destination .github -recurse -force
```
> NOTE - the `move-item` cmdlet does not recognize the `recurse` switch. the following command will simply not work:
```powershell
# move-item -recurse -force -literalpath skills -destination .github

```
> [!NOTE] from the upstream:
> __Work in Progress__ — This repository is under active development. Skills are being added and refined - they are inviting Contributions, feedback, and ideas to [Contributing](https://github.com/UiPath/skills/#contributing)


```sh
```
### Background 

UiPath Agent Skills give AI coding agents the domain knowledge to build, run, test, and deploy UiPath automations and agents — directly from your development environment. Each skill is a self-contained package of instructions and resources that teaches your coding agent how to perform a specific UiPath task.
## Quick Start

> **Prerequisite:** [Node.js](https://nodejs.org/) (LTS) is required — it includes `npm`.

```bash
npm -g install @uipath/cli
uip skills install
```
this command presumably inds *all* them AI coding agents installed on your machine and installs the skills for them, into each agent's directory, ready to use. If it can't find any agent, it prompts to target


### See Also 

  * [UiPath for Coding Agents - Skills overview](https://docs.uipath.com/coding-agents/standalone/latest/user-guide/skills-overview)

  * [Made a deep walkthrough of UiPath new Agent Skills](https://www.reddit.com/r/UiPath/comments/1tfn5ey/made_a_deep_walkthrough_of_uipath_new_agent_skills) . The actual URL Reddit puts into the `<iframe>` is [Build ANYTHING with UiPath Agent Skills (Full Tutorial)](https://www.youtube.com/watch?v=TSI7hWFqRZQ)

---
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

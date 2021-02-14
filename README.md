# SARL Controller for Agents in the City 2018+ - BASE

This project/package provides _basic_ SARL controllers for the [MAC Agents in City Contest](https://multiagentcontest.org/).

This version supports the [RMIT 2018+ edition](https://github.com/ssardina-agts/agtcity-server) of the Agents in City Game, which is basically the official 2018 edition with items brought back to shops.

It can be used as an initial set-up to build more advanced controllers.

## PREREQUISITES

The project relies on:


- Java Runtime Environment (JRE) and Java Compiler (javac) v1.8+. 
    - Tested with SUN Java 1.8 and OpenJDK 11.
- Maven project management and comprehension tool (to meet dependencies, compile, package, run).
- The [RMIT 2018+ game server edition](https://github.com/ssardina-agts/agtcity-server) (not the official 2018 server). This updated edition that brings back _items_ to _shop_ as in the 2017 version.
  
The following  dependencies are resolved via Maven and JitPack automatically:

- [SARL modules and execution engine](http://mvnrepository.com/artifact/io.sarl.maven).
- The [SARL Agents in City Middleware](https://github.com/ssardina-agts/agtcity-sarl-mw) which provides SARL capacity and skill for teams to connect and play in the game simulator.
  - Itself, the MW builds on the [EISMASSim](https://github.com/ssardina-agts/agtcity-server/tree/master/eismassim) environment interface connectivity that comes with the [MASSim Agents in City Server (RMIT 2018+ edition)](https://github.com/ssardina-agts/agtcity-server). This is a Java API that provides high-level access to the game sever to avoid dealing with low-level XML or JSON messages. The doc of the protocol and messages can be found [here](https://github.com/ssardina-agts/agtcity-server/blob/master/docs/eismassim.md).
- The [SARL-PROLOG-CAP](https://github.com/ssardina-agts/sarl-prolog-cap) project that provides a capacity and a skill for SARL agents to use [SWI-Prolog](http://www.swi-prolog.org/) and [JPL](https://jpl7.org/) for implementing the knowledge base of the agents.

### SWI-Prolog Configuration

One can use the SWI-Prolog + JPL that comes with a distribution or a locally compiled and installed one.

The current version (May 2020) is 8.2.+, which is stable and brings JPL 7.6.0 with a lot of fixes and improvements form the 7.6.x version that comes with default distros. The latest stable version can be obtained via this [PPA](https://www.swi-prolog.org/build/PPA.html). In principle, nothing else is needed if we are to use this version.

To tell the system to use a a locally compiled and installed version of SWI-Prolog with JPL package (for example in `/usr/local/swipl-git/lib/swipl/`):

```bash
export SWI_HOME_DIR=/usr/local/swipl-git/lib/swipl/
export LD_LIBRARY_PATH=$SWI_HOME_DIR/lib/x86_64-linux/:$LD_LIBRRY_PATH
```

Observe while either framework brings also the `jpl.jar` Java API implementation, the SARL-PROLOG-CAPACITY will gather and use the JAR file from the [JPL official repository](https://github.com/SWI-Prolog/packages-jpl).

## INSTALL, RUN and DEVELOP

You can run the SARL controller, either from ECLIPSE or from CLI (via Java or Maven), please refer to [this instructions](https://gist.github.com/ssardina/43d6e6f469921e5f692b37304f952d43#4-running-a-sarl-application).

You can use the source JAR files for modules [EIS](https://github.com/eishub/eis) and [massim](https://github.com/eishub/massim) provided in `extras/` to attah sources in ECLIPSE (their sources are not available via Maven).


So, to **run the system** you need to follow these general steps:

1. **Start RMIT 2018+ Game Server**. From `server/` folder:

	```bash
	$ ./startServer.sh conf/SampleConfig.json
	```

	or run it using Java itself:

	```bash
	$ java -jar target/server-2020-1.0-jar-with-dependencies.jar --monitor 8001 -conf conf/SampleConfig.json
	```

	Note that the configuration file (here, `conf/SampleConfig.json`) makes a reference to the team configuration file at the bottom (e.g., `conf/teams/A.json`) which is the file containing all agents allowed to connect and with which id and password. These are the ones your system will use in your agent configuration file.

	In the console of the server, you will see a URL link to the monitor. Click it to see the GUI interface of the game.

2. Start the SARL Controller, either via ECLIPSE or through the CLI. Remember the application needs the JSON server connection configuration file). For example.

	```bash
	$ mvn exec:java -Dexec.args="SuperSingleAgent conf/SingleAgent" -Dloglevel=4
	```

	If you run `mvn exec:java` without arguments, it will list all available controllers and ask for one (and also ask for location of server connection configuration file).
	
	You can use grep inverse `-e` to get rid of all the printout of XML messages produced by the `EI` framework (filter out everything between `< >`, and any "`sent`" and "`received`" printout at start of line):

 	```bash
	$ mvn exec:java | grep -v -e  \<.*\> -e WARNING -e '^ sent' -e '^ received'
	```

	If you packaged the JAR with dependencies:

	```bash
	$ java -jar target/agtcity-sarl-base-4.5.0.11.0-jar-with-dependencies.jar io.sarl.sre.boot.Boot SuperSingleAgent -Dloglevel=4
	```

3. Start the MASSIM Simulation by just hitting **_ENTER_** in the Game Server console.
4. Enjoy! You should start seeing the agent reporting things in the console. You can see the simulation on the web browser.

## Developing SARL agents

Package `au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy` contains some simple examples that could be used as a base for your own new development. In particular, it has all the connectivity/communication with the game server already resolved, as well as a simple sensing cycle.

As the system becomes very complex, it may be convenient to use a better technology than Java data-structures to do belief maintenance and reasoning. One possibility is to use Prolog knowledge-bases, by making use of [SWI-Prolog](https://www.swi-prolog.org/) via the [JPL](https://jpl7.org/)-based skill `SWIJPL_KB_Prolog` in the [SARL-PROLOG-CAP](https://github.com/ssardina-agts/sarl-prolog-cap) framework. Some initial code for a domain knowledge base is already provided in agent `SWISingleFullAgent`, including substantial Prolog code to process percepts.

Three simple example SARL agent controllers are provided in this base system as a reference. These examples should provide a solid base for developing more sophisticated agent systems for Agents in City.

All the agents can be run via the booting class `BootMAS`, which is the default execution class in the package JAR file. One has to give the name of the agent controller to start as an argument.

To change the log level, pass `-Dloglevel=n` option (FINE/DEBUG=4; INFO=3; WARNING=2; ERROR=1). Defaults to level 3.

### **`SuperSingleAgent`**

_Package_: `au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.SuperSingleAgent`

A simple centralized agent that controls _many entities_ in the MW. It can be started via:

```bash
$ mvn exec:java -Dexec.args="SuperSingleAgent conf/SingleAgent"
```

This SARL agent will first register a set of entities to be controlled. Then it will repetitively perceptive percepts for each entity, update a global its global view of the game (by updating a MW's `State` object), and make all entities navigate to random shops.

### **`SWISuperSingleAgent`**

_Package_: `au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.SWISingleFullAgent`

This is a similar system as `SWISuperSingleAgent` but uses Prolog  Knowledge Base (KB) via the JPL-based `SWI_KB_AgtCity` skill.

To help understand the Prolog KB, the SARL agent prints out, at every sensing cycle, some results from queries and then dumps its entire KB into a file with the step number as part of the file name.

The agent contains a few example uses of how to assert in the KB and query it. It also contains simple logic to continuously select a random destination and go there.

This agent can be run directly as follows:

```java
$ mvn -o exec:java -Dexec.args="SWISuperSingleAgent conf/SingleAgent"
```

### **`Multi Agent System`**

This is a multi SARL agent team started with agent which starts a set of agents of type **SWISuperSingleAgent**.

Each `SWISuperSingleAgent` spawn is assigned a configuration file to control a set of entities in the simulation. To do so, the **BootMultiSWIAgents** overrides the default values of **`SWISuperSingleAgent`** to provide authentication files directly as arguments to the initialization.

----------------------------
## PROJECT CONTRIBUTORS

* Sebastian Sardina (Project leader and contact - ssardina@gmail.com)
* Matthew McNally (first version of SWI-based agent via the deprecated **Mochalog** interface)

## LICENSE ##

This project is using the GPLv3 for open source licensing for information and the license visit GNU website (https://www.gnu.org/licenses/gpl-3.0.en.html).

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
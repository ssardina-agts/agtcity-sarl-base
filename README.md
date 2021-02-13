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
* The [SARL-PROLOG-CAP](https://github.com/ssardina-agts/sarl-prolog-cap) project that provides a capacity and a skill for SARL agents to use [SWI-Prolog](http://www.swi-prolog.org/) and [JPL](https://jpl7.org/) for implementing the knowledge base of the agents.

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

1. Start MAC17 Game Server. For example, from `server/` subdir:

		java -jar target/server-2017-0.7-jar-with-dependencies.jar --monitor 8001 -conf conf/Mexico-City-Test.json


	Please note the following important aspects of your game server configuration:
		
	* The configuration file (here, `conf/Mexico-City-Test.json`) makes a reference to the team configuration file at the bottom (e.g., `conf/teams/A.json`) which is the file containing all agents allowed to connect and with which id and password. These are the ones your system will use in your agent configuration file.
	* Configure the game server so that Prolog compatible terms are generated. In particular it is important to have names not starting with capital letters, as they will be undertood as variables. 
		* Rename "2017-Mexico-City-Test` to `2017-mexico-city-test`.
		* Rename team names. Instead of team `A`, use team `teamA`.	
		

2. Start the SARL Controller, either via ECLIPSE or through the CLI (again, see [general SARL instructions](https://gist.github.com/ssardina/43d6e6f469921e5f692b37304f952d43#4-running-a-sarl-application)).
	* System will generally need a JSON configuration file for the game server. 
	* By default, the JAR file built does not carry all dependencies as the compilation is too slow. Hence you need to execute via Maven execution plugin, which will run the default `BootMAS` class:
	
			mvn exec:java 
			mvn exec:java -Dexec.args="SingleFullAgent conf/SingleAgent" -Dloglevel=4
			mvn exec:java -Dexec.args="SWISingleFullAgent conf/SingleAgent" -Dloglevel=4
			mvn exec:java -Dexec.args=BootMultiSWIAgents conf/MultiAgents/" -Dloglevel=4
	
	
	If you run without arguments, it will list all available controllers and ask for one at console.
	
	* You can use grep inverse to get rid of all the printout of XML messages produced by the EI framework:
	
	       mvn exec:java | grep -v -e  \<.*\> -e WARNING -e '^ sent' -e '^ received' 
	       
	    This will filter out everything between < >, and any "sent" and "received" printout at start of line.
		
	* If you package the JAR with dependencies. then:

			java -jar target/sarl-agtcity-base-1.5.0.7.2-jar-with-dependencies.jar SWISingleFullAgent -Dloglevel=4

		or via the SARL booting class `io.janusproject.Boot`:	
	
			java -cp target/sarl-agtcity-base-1.5.0.7.2-jar-with-dependencies.jar io.janusproject.Boot au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.SWISingleFullAgent -Dloglevel=4
	* You may want to grep to avoid the logging of all the XML messages: `grep -v \<.*\>` 
			
			
3. Start the MASSIM Simulation by just hitting *ENTER* in the Game Server console
4. Enjoy! You should start seeing the agent reporting things in the console. 
    * You can see the simulation on the web browser.


### Developing SARL agents

Package `au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy` contains some simple examples that coul dbe used to start your new development. In particular, it has all the connectivity/communication with the game server already resolved, as well as a simple sensing cycle.

As the system becomes very complex, it is covenient to rely on Prolog knowledgebases (via JPL-based skill `SWIJPL_KB_Prolog`) rather than storing all information in plan Java data-structures. We suggest to carefully underestand the [SARL PROLOG CAP](https://github.com/ssardina-agts/sarl-prolog-cap) framework, which provides the skill to manipulate a Prolog knowledgebase. Some initial code for a domain knowledge base is already provided in this base system, including substantial Prolog code to process all percepts.

## EXAMPLE AGENTS 

These are all "thin" agent controllers for the players in the game, but they should provide a solid base for developing more sophisticated agent systems.

All the agents can be run via the booting class `BootMAS`, which is the default execution class in the package JAR file. One has to give the name of the agent controller to start as an argument.

To change the log level, pass `-Dloglevel=n` option (FINE/DEBUG=4; INFO=3; WARNING=2; ERROR=1). Defaults to level 3.

The agent examples are:

1. **SuperSingleAgent**: A simple central agent controlling _all players_ in the simulator, receiving and processing the sensing from the environment, and then making all players navigate to random facilities. It uses the Java Percept Aggregator facility provided in the MW as percepts from players have a lot of shared content.
	* _Class_: `au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.SuperSingleAgent`
	* This is the **default** agent controller for `BootMAS` class so it can be run by just doing
	* To run: 
		* Using Maven: `mvn exec:java -Dexec.args=SuperSingleAgent -Dloglevel=4`
		* Via main `BootMAS` class: `java -jar target/sarl-agtcity-base-1.5.0.7.2-jar-with-dependencies.jar  SuperSingleAgent`
		* Via SARL booting class: `java -cp target/sarl-agtcity-base-1.3.0.7.2-jar-with-dependencies.jar io.janusproject.Boot au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.SuperSingleAgent`
		* Then, select the location where `eismassimconfig.json` server configuration file is located.

2. **SWISingleFullAgent**: This is one single SARL agent controlling _all players_ in the game and, importantly, using an SWI Prolog Knowledge Base via the JPL-based skill.
	* _Class_: `au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.SWISingleFullAgent`
	* To help understand how the SWI Knowledge Base is updated, every percept cycle the agent prints out some results from queries and then dumps its entire KB into file `swiSingleFullAgent-<n>.pl`, where `<n>` is the step number. The agent contains a few example uses of how to assert in the KB and query it. It also contains simple logic to continously select a random destination and go there. For achieving this, both SARL (emitting and handling event **E_MoveRandomly**) and SWI Prolog (for choosing the destination) is used.
	* To run this agent: `mvn exec:java -Dexec.args=SWISingleFullAgent -Dloglevel=4`, and then select the location where `eismassimconfig.json` server configuration file is located.

3. **Multi Agent System**: This is a multi SARL agent team started with agent **BootMultiSWIAgents** which starts a set of agents of type **SWISingleFullAgent**. Each of them is assigned a configuration file to control a set of player  in the simulation. To do so, the **BootMultiSWIAgents** overrides the default values of **SWISingleFullAgent** to provide authentication files directly as arguments to the initialization.
	* _Class_: `au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.BootMultiSWIAgents`
	* This is a perfect example of versatility where a SARL agent can be given the task to control all players or just a subset. 
	* To run this agent: 
		* Using Maven: `mvn exec:java -Dexec.args=BootMultiSWIAgents -Dloglevel=4` 
		* Using the main class `BootMAS` (after compiling with all dependencies): `java -jar target/sarl-agtcity-base-1.3.0.7.2-jar-with-dependencies.jar BootMultiSWIAgents`
	* Then, select the location where `eismassimconfig.json` server configuration file is located.



## PROJECT CONTRIBUTORS ##

* Sebastian Sardina (Project leader and contact - ssardina@gmail.com)
* Matthew McNally (first version of SWI-based agent via the old Mochalog interface)


## LICENSE ##


This project is using the GPLv3 for open source licensing for information and the license visit GNU website (https://www.gnu.org/licenses/gpl-3.0.en.html).

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.

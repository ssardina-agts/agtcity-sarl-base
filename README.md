# SARL Agent Controller for MAC Agents in the City 2017 - BASE #

This project/package provides _basic_ SARL controllers for the [2017 MAC Agents in City Contest](https://multiagentcontest.org/). It can be used as an initial set-up to build more advanced controllers.


The project relies on:

1. The [MASSIM Agents in City SARL Connectivity Interface](https://bitbucket.org/ssardina-research/sarl-agtcity-intf).
	* Java-code providing a more high-level access to the game sever than low-level JSON messages.
2. The [SARL Agents in City Middleware](https://bitbucket.org/ssardina-research/sarl-agtcity-mw) which provides SARL capacity and skill for teams to connect and play in the game simulator.
2. The [SARL-PROLOG-CAP](https://bitbucket.org/ssardina-research/sarl-prolog-cap) project that provides a capacity and a skill for SARL agents to use [SWI Prolog](http://www.swi-prolog.org/) for implementing the knowledge base of the agents. 

Two dummy controllers, one of them using SWI Prolog, are provided.

**IMPORTANT:** A comprehensive set of instructions how to run SARL systems can be found [here](https://bitbucket.org/snippets/ssardina/6eybMg/sarl-application-general-information-setup)


## PREREQUISITES

* Java Runtime Environment (JRE) and Java Compiler (javac) v1.8 (Sun version recommended)
* Maven project management and comprehension tool (to meet dependencies, compile, package, run).
* SARL (SRE Janus) execution engine:
	* Version defined via environment variable `SARL_VERSION` (make sure it is defined to the version to be used, e.g., 0.7.2)
	* Version tested: 0.6.1, 0.7.2.
	* Obtained via Maven automatically from <http://mvnrepository.com/artifact/io.sarl.Maven>
* The [SARL Agents in City SARL Middleware](https://bitbucket.org/ssardina-research/sarl-agtcity-mw). 
	* Should be obtained automatically via Maven + Jitpack.
	* Middleware uses the [Java EIS Hub project](https://github.com/eishub), an abstract interface to implement communication with the environment. In particular, modules [EIS](https://github.com/eishub/eis) and [massim](https://github.com/eishub/massim) are used. Their JAR sources are provided under `extras/` so they can be used to attach sources in ECLIPSE.
* The [SARL-PROLOG-CAP](https://bitbucket.org/ssardina-research/sarl-prolog-cap) capacity+skill for [SWI Prolog](http://www.swi-prolog.org/) system:
	* Capacity and skill to allow the use of SWI Prolog knowledge-bases in SARL agents.
	* Relies on [Mochalog](https://github.com/ssardina/mochalog) and [JPL](https://jpl7.org/) to have Prolog access from Java.
	* The right version (specified in the POM file) should be obtained automatically via [JitPack](https://jitpack.io/#org.bitbucket.ssardina-research/sarl-prolog-cap).
	* **IMPORTANT**: Please refer to the instructions and examples in the [capacity+skill's page](https://bitbucket.org/ssardina-research/sarl-prolog-cap) to set-up and use it in your SARL application.

## INSTALL, RUN and DEVELOP

You can run the SARL controller, either from ECLIPSE or from CLI (via Java or Maven), please refer to [this instructions](https://bitbucket.org/snippets/ssardina/6eybMg#markdown-header-4-running-the-sarl-application).

You can use the source JAR files for modules [EIS](https://github.com/eishub/eis) and [massim](https://github.com/eishub/massim) provided in `extras/` to attah sources in ECLIPSE (their sources are not available via Maven).


So, to **run the system** you need to follow these general steps:

1. Start MAC17 Game Server. For example, from `server/` subdir:

		java -jar target/server-2017-0.7-jar-with-dependencies.jar --monitor 8001 -conf conf/Mexico-City-Test.json

	Note that the configuration file (here, `conf/Mexico-City-Test.json`) makes a reference to the team configuration file at the bottom (e.g., `conf/teams/A.json`) which is the file containing all agents allowed to connect and with which id and password. These are the ones your system will use in your agent configuraition file.

2. Start the SARL Controller, either via ECLIPSE or through the CLI (again, see [general SARL instructions](https://bitbucket.org/snippets/ssardina/6eybMg#markdown-header-4-running-the-sarl-application)).
	* System will generally need a json configuration file for the game server and one for the teams. 
	* One way to run it from CLI directly once it has been packaged is as follows:
	
		```
		java -cp target/sarl-agtcity-extras-1.0.0.7.2-jar-with-dependencies.jar io.janusproject.Boot au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.SWISingleFullAgent
		```
		
3. Start the MASSIM Simulation by just hitting *ENTER* in the Game Server console
4. Enjoy! You should start seeing the agent reporting things in the console. 
    * You can see the simulation on the web browser.



## EXAMPLE AGENTS ##

These are all thin agent controllers for the players in the game, but they should provide a solid base for developing more sophisticated agent systems.


### SuperSingleAgent

A simple central agent controlling all players in the simulator, receiving and processing the sensing from the environment, and then making all players navigate to random facilities.

It uses the Java Percept Aggregator facility provided in the MW as percepts from players have a lot of shared content.

All data is stored in Java.


### SWISingleFullAgent 

This is one single SARL agent controlling all players in the game and, importantly, using an SWI Prolog Knowledge Base via the [Mochalog Framework](https://github.com/ssardina/mochalog).

To help understand how the SWI Knowledge Base is updated, every percept cycle the agent prints out some results from queries and then dumps its entire KB into file `swiSingleFullAgent-<n>.pl`, where `<n>` is the step number.

The agent contains a few example uses of how to assert in the KB and query it. 

It also contains simple logic to continously select a random destination and go there. For achieving this, both SARL (emitting and handling event **E_MoveRandomly**) and SWI Prolog (for choosing the destination) is used.

To run this agent, you can do:

```
java -cp target/sarl-agtcity-extras-1.0.0.7.2-jar-with-dependencies.jar io.janusproject.Boot au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.SWISingleFullAgent
```

or simply, if `io.janusproject.Boot` is set as the main class in execution plugin:

```
java -jar target/sarl-agtcity-extras-1.0.0.7.2-jar-with-dependencies.jar au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.SWISingleFullAgent
```


### Multi Agent System

This is a multi SARL agent team started with agent **BootMultiSWIAgents** which starts a set of agents of type **SWISingleFullAgent**. Each of them is assigned a configuration file to control a set of player  in the simulation.

To do so, the **BootMultiSWIAgents** overrides the default values of **SWISingleFullAgent** to provide authentication files directly as arguments to the initialization.

This is a perfect example of versatility where a SARL agent can be given the task to controll all players or just a subset. 

To run this agent, you can do:

```
java -cp target/sarl-agtcity-extras-1.0.0.7.2-jar-with-dependencies.jar io.janusproject.Boot au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.BootMultiSWIAgents
```

or simply, if `io.janusproject.Boot` is set as the main class in execution plugin:

```
java -jar target/sarl-agtcity-extras-1.0.0.7.2-jar-with-dependencies.jar au.edu.rmit.agtgrp.agtcity.sarl.agents.dummy.BootMultiSWIAgents
```



## LINKS 

For general links check [here](https://bitbucket.org/snippets/ssardina/6eybMg#markdown-header-1-software-prerequisites-and-links).

* The Multi Agent Agents in City 2017 contest:
	* Multi-Agent Contest Home Page: https://multiagentcontest.org/
	* Main git repository: https://github.com/agentcontest/massim
	* Documentation: https://github.com/agentcontest/massim/tree/master/docs
	* EISMASSim Documentation (the interface provided to communicate with game server): https://github.com/agentcontest/massim/blob/master/docs/eismassim.md
		* Web page of the Environment Interface Standard (EIS): https://github.com/eishub/




## PROJECT CONTRIBUTORS ##

* Sebastian Sardina (Project leader and contact - ssardina@gmail.com)
* Matthew McNally (first version of SWI-based agent via Mochalog)


## LICENSE ##


This project is using the GPLv3 for open source licensing for information and the license visit GNU website (https://www.gnu.org/licenses/gpl-3.0.en.html).

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
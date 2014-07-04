import sbt._

class JpaScalaProject(info: ProjectInfo) extends DefaultProject(info) {

	val jbossRep = "JBoss repository" at "http://repository.jboss.org/nexus/content/groups/public-jboss/"


	val jbossVersion = "3.5.6-Final"
	val springVersion = "3.0.5.RELEASE"

	val depHibernateCore = "org.hibernate" % "hibernate-core" % jbossVersion 
	val depHibernateEnti = "org.hibernate" % "hibernate-entitymanager" % jbossVersion
	val depHibernateValidator = "org.hibernate" % "hibernate-validator" % "4.1.0.Final"  
	val depJavaxValidation = "javax.validation" % "validation-api" % "1.0.0.GA"  


	val depSlf4JApi = "org.slf4j" % "slf4j-api" % "1.6.1"
	val depSlf4JLog4j12 = "org.slf4j" % "slf4j-log4j12" % "1.6.1"
	val depLog4J = "log4j" % "log4j" % "1.2.16"

	val springExp    = "org.springframework" % "spring-expression" % springVersion
	val springBeans  = "org.springframework" % "spring-beans" % springVersion
	val springCore   = "org.springframework" % "spring-core" % springVersion
	val springCtx    = "org.springframework" % "spring-context" % springVersion
	val springCtxSup = "org.springframework" % "spring-context-support" % springVersion
	val springJdbc   = "org.springframework" % "spring-jdbc" % springVersion
	val springOrm    = "org.springframework" % "spring-orm" % springVersion
	
	val hsqldb = "hsqldb" % "hsqldb" % "1.8.0.7"

}

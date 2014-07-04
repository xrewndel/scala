package hbnt

import org.springframework.context.support.ClassPathXmlApplicationContext

object HibernateScala {
  def main( args : Array[String]) {

    val ctx = new ClassPathXmlApplicationContext("application-context.xml")
    val dao: CustomerDao = ctx.getBean(classOf[CustomerDao])

    dao.save(new Customer("Paul", "Hildebrand"))
    dao.save(new Customer("Floor", "Hildebrand"))
    dao.save(new Customer("Storm", "Hildebrand"))
    dao.save(new Customer("Jan", "Jansen"))
    dao.save(new Customer("Peter", "Jansen"))

    println(dao.getAll)
    println(dao.getByLastName("Jansen"))
    println(dao.getByLastName("Hildebrand"))

	(1 to 10) foreach (y =>
    dao.find(y) match {
        case Some(x) => println(x)
        case None => println("No customer found with id " + y)
    }
	)
    

	
  }

}
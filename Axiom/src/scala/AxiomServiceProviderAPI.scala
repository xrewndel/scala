package net.fmb.integrator.serviceprovider.axiom

import net.fmb.integrator.core.serviceprovider.{ AbstractServiceProviderAPI, ValidationError, Validations }

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

/**
 * Implementation Axiom
 */
class AxiomServiceProviderAPI extends AbstractServiceProviderAPI {
  import AbstractServiceProviderAPI.ConfigMeta
  
  /**
   * Config
   * @return Container with requirements to configuration
   */
  override def declareConfig(): ConfigMeta = (
    new ConfigMeta
      // declares requirements
      // for config
      //
      // 	requires "login"
      //	requires "password"
      //	opts ("debug", "0")
  )
	
		
  /**
   * Extract identifier of user request
   *
   * @param params Map of parameters
   * @return
   */
  override def extractIdentity(params: java.util.Map[String, String]): String = {
    // STUB
    //
    // should return identifier from
    // input map
    //
    // example:
    params.get("phone")
  }
	
	
  /**
   * Check request for validity locally
   *
   * @param params Map of input parameters
   * @param complete True = validate full request data, False = only identity
   * @return void
   * @throws ValidationError
   */
  override def validateLocal(params: java.util.Map[String, String], complete: Boolean) {
    /* Identity */
    // local validations of identity:
    // Validations.requireParams(List("phone").asJava, params)
    // regexps, etc

    /* other */
    // if (complete) {
    //   validation of other request parameters:
    //   Validations.requireParams(List("amount", ...).asJava, params)
    //   val amount = Validations.getAmount(params)
    //   if (amount > 1000.00D) throw new ValidationError(ValidationError.INVALID_VALUE, "Amount is to big, 1000 max")
    //   etc, etc
    // }
  }
  
  
  /**
   * Check request for validity remotely (nothing in our case)
   *
   * @param params Map of input parameters
   * @param complete True = validate full request data, False = only identity
   * @return void
   * @throws ValidationError
   * @throws Exception
   */
  override def validateRemote(params: java.util.Map[String, String], complete: Boolean) {
    // Follow the same pattern as validateLocal, but intent is to use validation possibilities from the provider
  }
	
	
  /**
   * Execute request
   *
   * @param params
   * @return
   * @throws Exception
   */
  override def execute(params: java.util.Map[String, String]): java.util.Map[String, String] = {
    // should execute actual request against remote API
    // and return map of params

    params
  }


  /**
   * Returns balance + currency code pair
   *
   * @return Tuple class BalanceTuple(amount, iso3 currency code)
   * @throws Exception
   */
  /*
  override def getBalance() = {
    // uncomment and implement if provider allows checking agent's balance:
    // new BalanceTuple(balance, currency_code);
  }
  */


  /**
   * Self-test of remote endpoint
   *
   * @return void If success
   * @throws Exception
   */
  /*
  override def selfTest() {
    // uncomment if API has capability to test configuration or may report about maintenance -
    // implement functionality here, throw exceptions as a signal of error
    // example: rawAPI.ping()
  }
  */


  /**
   * This method may serve for information exchange
   *
   * @param params Map of input parameters
   * @return Map of strings, depends on context
   * @throws Exception
   */
  /*
  override def getInfo(params: java.util.Map[String, String]): java.util.Map[String, String] = {
    // uncomment and implement if you need to exchange
    // data to prepare payment, or to provide useful information;
    // for example method might return list of denominations,
    // currency course, fetch amount by some reference, etc
  }
  */
}
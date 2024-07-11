
import java.sql.ShardingKey
import java.util.Locale

class TaxCalculator {

  sealed trait CapitalGainType
  case object Property extends CapitalGainType

  //  Tax bands for capital gain
  private val capitalGainAllowance: Int = 3000

  private val basicRateProperty: Double = 0.18
  private val highRateProperty: Double = 0.24
  private val basicRateNotProperty: Double = 0.1
  private val highRateNotProperty: Double = 0.2


  // Tax bands (simplified to make testing a bit easier)
  private val personalAllowance: Int = 10000
  private val basicRateLimit: Int = 50000
  private val higherRateLimit: Int = 125000

  // Tax rates
  private val personalAllowanceRate: Double = 0
  private val basicRate: Double = 0.2
  private val higherRate: Double = 0.4
  private val additionalRate: Double = 0.45

  // A method to calculate the total amount of tax to be paid, returned as a double
  def calculateTax(income: Double): Double = {
    val basicTaxible = (basicRateLimit - personalAllowance)*basicRate
    val higherTaxible = (higherRateLimit - basicRateLimit)*higherRate

    if (income <= personalAllowance){
      income * personalAllowanceRate
    }
    else if (income <= basicRateLimit){
      (income-personalAllowance)*basicRate
    }
    else if (income <= higherRateLimit){
      (income-basicRateLimit)*higherRate + basicTaxible
    }
    else if (income > higherRateLimit){
      (income-higherRateLimit)*additionalRate + higherTaxible + basicTaxible
    }
    else{-1.0}
  }


  // A method which can tell you if someone is a higher rate taxpayer
  def isHigherRateTaxpayer(income: Double): Boolean = {
    income > basicRateLimit
  }

  // A method that will return a string with the income limit of their current tax band.
  // The return will also be formatted, E.g: "£12,500" or "No limit"
  def formattedCurrentTaxAllowance(income: Double): String = {
    val formatter = java.text.NumberFormat.getCurrencyInstance(Locale.UK)
    if (income <= personalAllowance) {
      formatter.format(personalAllowance)
    } else if (income <= basicRateLimit) {
      formatter.format(basicRateLimit)
    } else if (income <= higherRateLimit) {
      formatter.format(higherRateLimit)
    } else if (income > higherRateLimit) {
      "No Limit"
    }
    else {
      "Error"
    }
  }

  def shareTax(income:Double, shareGains:Double):Double = {
    val totalIncome:Double = income + shareGains

    if (shareGains <= capitalGainAllowance) {0}
    else if (shareGains > capitalGainAllowance){
      if(isHigherRateTaxpayer(totalIncome)){ (shareGains - capitalGainAllowance)*highRateNotProperty}
      else {(shareGains - capitalGainAllowance)*basicRateNotProperty}
    } else{-1.0}
  }

  def totalTax(income:Double, shareGains:Double):Double = {
    calculateTax(income) + shareTax(income, shareGains)
  }

}

import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TaxCalculatorSpec extends AnyWordSpec {

  val taxCalculator: TaxCalculator = new TaxCalculator

  // I've done the first test for you!
  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when {
      "the income is below the personal tax limit" in {
        val result: Double = taxCalculator.calculateTax(9000)
        assert(result == 0.0)
      }
      "the income is below the basic rate limit" in {
        val result: Double = taxCalculator.calculateTax(15_000)
        assert(result == 1_000.0)
      }
      "the income is below the higher rate limit" in {
        val result: Double = taxCalculator.calculateTax(60_000)
        assert(result == 12_000.0)
      }
      "the income is above the higher rate limit" in {
        val result: Double = taxCalculator.calculateTax(130_000)
        assert(result == 40_250.0)
      }
    }
  }
  "TaxCalculator.isHigherRateTaxpayer" should {
    "return true if someone is a higher rate taxpayer " when {
      "the income is below the basic tax limit" in {
        assert(!taxCalculator.isHigherRateTaxpayer(10000))
      }
      "the income is the basic tax limit" in {
        assert(!taxCalculator.isHigherRateTaxpayer(50000))
      }
      "the income is above the higher rate limit" in {
        assert(taxCalculator.isHigherRateTaxpayer(100000))
      }
    }
  }
  "TaxCalculator.formattedCurrentTaxAllowance" should {
    "return a formatted string with the income limit of the current tax band" when {
      "the income is below the personal allowance rate tax limit" in {
        assert(taxCalculator.formattedCurrentTaxAllowance(5000.0) == "£10,000.00")
      }
      "the income is below the basic tax limit rate tax limit" in {
        assert(taxCalculator.formattedCurrentTaxAllowance(11_000.0)=="£50,000.00")
      }
      "the income is the basic tax limit" in {
        assert(taxCalculator.formattedCurrentTaxAllowance(50_000.0)=="£50,000.00")
      }
      "the income is above the basic rate limit" in {
        assert(taxCalculator.formattedCurrentTaxAllowance(75_000.0)=="£125,000.00")
      }
      "the income is above the higher rate limit" in {
        assert(taxCalculator.formattedCurrentTaxAllowance(150_000.0)=="No Limit")
      }
      "the income is negative" in {
        assert(taxCalculator.formattedCurrentTaxAllowance(-100)=="£10,000.00")
      }
    }
  }

  "TaxCalculator.shareTax" should {
    "return the capital gain tax to pay " when {
      "the share gains are below the capital gain allowance and total income > basic tax limit" in {
        assert(taxCalculator.shareTax(income = 60_000, shareGains = 2000) == 0.0)
      }
      "the share gains are below the capital gain allowance and total income < basic tax limit" in {
        assert(taxCalculator.shareTax(income = 25_000, shareGains = 2000) == 0.0)
      }
      "the share gains are equal to the capital gain allowance and total income > basic tax limit" in {
        assert(taxCalculator.shareTax(income = 60_000, shareGains = 3000) == 0.0)
      }
      "the share gains are equal to the capital gain allowance and total income < basic tax limit" in {
        assert(taxCalculator.shareTax(income = 25_000, shareGains = 3000) == 0.0)
      }
      "the share gains are > capital gain allowance and total income < basic tax limit" in {
        assert(taxCalculator.shareTax(income = 25_000, shareGains = 4000) == 100)
      }
      "the share gains are > the capital gain allowance and total income > basic tax limit" in {
        assert(taxCalculator.shareTax(income = 60_000, shareGains = 4000) == 200)
      }
    }
  }



  "TaxCalculator.totalTax" should {
    "return the income tax and capital gain tax to pay " when {

// ################  INCOME <= PERSONAL ALLOWANCE  ################
      "total income </= personal allowance and the share gains < capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 5000, shareGains = 2000) == 0.0)
        assert(taxCalculator.totalTax(income = 10_000, shareGains = 2000) == 0.0)
      }
      "income </= personal allowance and the share gains = capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 5_000, shareGains = 3000) == 0.0)
        assert(taxCalculator.totalTax(income = 10_000, shareGains = 3000) == 0.0)
      }
      "income </= personal allowance and the share gains > capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 5_000, shareGains = 4000) == 100.0)
        assert(taxCalculator.totalTax(income = 10_000, shareGains = 4000) == 100.0)
      }

      // ################  INCOME <= BASIC TAX LIMIT  ################
      "income </= basic tax limit and the share gains < capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 15_000, shareGains = 2000) == 1_000)
        assert(taxCalculator.totalTax(income = 50_000, shareGains = 2000) == 8_000)
      }
      "income </= basic tax limit and the share gains = capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 15_000, shareGains = 3000) == 1_000)
        assert(taxCalculator.totalTax(income = 50_000, shareGains = 3000) == 8_000)
      }
// ################  INCOME <= BASIC TAX LIMIT  &&  INCOME + CAPITAL GAINS >= BASIC TAX LIMIT ################
      "income </= basic tax limit and the share gains > capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 15_000, shareGains = 4000) == 1_100)
        assert(taxCalculator.totalTax(income = 50_000, shareGains = 4000) == 8_200)
      }

      // ################  INCOME <= HIGHER TAX LIMIT  ################
      "income </= higher tax limit and the share gains < capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 60_000, shareGains = 2000) == 12_000)
        assert(taxCalculator.totalTax(income = 125_000, shareGains = 2000) == 38_000)
      }
      "income </= higher tax limit and the share gains = capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 60_000, shareGains = 3000) == 12_000)
        assert(taxCalculator.totalTax(income = 125_000, shareGains = 3000) == 38_000)
      }
      "income </= higher tax limit and the share gains > capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 60_000, shareGains = 4000) == 12_200)
        assert(taxCalculator.totalTax(income = 125_000, shareGains = 4000) == 38_200)
      }

      // ################  INCOME > HIGHER TAX LIMIT  ################
      "income > higher tax limit and the share gains < capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 130_000, shareGains = 2000) == 40_250)
      }
      "income > higher tax limit and the share gains = capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 130_000, shareGains = 3000) == 40_250)
      }
      "income > higher tax limit and the share gains > capital gain allowance" in {
        assert(taxCalculator.totalTax(income = 130_000, shareGains = 4000) == 40_450)
      }
    }
  }
}

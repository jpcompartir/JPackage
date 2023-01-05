#' Rough estimate of take-home salary for different patypoints with student loan and pension taken into account.
#'
#' @param salary Gross salary
#' @param pension Contributing to a pension or not? If TRUE then value is 0.05 * taxable income(may edit this to be a prop.)
#' @param student_loan Repaying a student loan or not? If TRUE then value is 0.05 (may edit this to be a prop.)
#' @param tax_free_allowance
#'
#' @return integer with net pay per year (/12 for monhtly)
#' @export
#'
net_income <- function(salary, pension = TRUE, student_loan = TRUE, tax_free_allowance = 12570){

  high_bracket <- 50270
  extra_bracket <- 150000

  taxable_income <- salary - tax_free_allowance


  pension_contribution_annual <- 0
  if(pension){
    pension_contribution_annual <- taxable_income * 0.05
    # pension_contribution_mensual <- pension_contribution_annual/12
  }

  student_loan_contribution_annual <- 0
  if(student_loan){
    student_loan_contribution_annual <- 0.05 * taxable_income
    # student_loan_contribution_mensual <- student_loan_contribution_annual / 12
  }

  if(salary <= high_bracket){
    basic_rate_taxable <-  salary - tax_free_allowance
  } else{basic_rate_taxable <- high_bracket - tax_free_allowance}

  higher_rate_taxable <- salary - high_bracket
  extra_rate_taxable <- salary - extra_bracket

  if(salary <= high_bracket){
    income_tax <- 0.2 * basic_rate_taxable
  } else if(salary > high_bracket){
    income_tax <- (0.2 * basic_rate_taxable) + (0.4 * higher_rate_taxable)
  } else if(salary > extra_bracket){
    income_tax <- (0.2 * basic_rate_taxable) + (0.4 * higher_rate_taxable) + (0.45 * extra_rate_taxable)
  }

  calculate_ni <- function(salary){
    free_allowance <- 242*52

    ni_deductable <- salary - free_allowance

    if(salary <= 52988){
      ni <- ni_deductable * 0.1325
    }else{
      higher_portion <- 40404 * 0.1325

      lower_portion <- (salary - 52988) * 0.0325

      ni <- higher_portion + lower_portion
    }
    return(ni)

  }

  national_insurance <- calculate_ni(salary)

  deductions <- income_tax + pension_contribution_annual + student_loan_contribution_annual + national_insurance

  net_income <- (taxable_income -  deductions) + tax_free_allowance

  return(net_income)
}

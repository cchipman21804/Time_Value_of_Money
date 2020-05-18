      *****************************************************************
      * FIND P GIVEN F                                                *
      *                                                               *
      * A simple program that calculates the Present Value of an      *
      * investment after the user enters the Future Value, the        *
      * annual interest rate, and the term of the investment.         *
      *                                                               *
      *****************************************************************

       identification division.
       program-id.   fpgf.

       data division.
       working-storage section.

      *****************************************************************
      *                                                               *
      * Present Value formula:                                        *
      *                                                               *
      *              P = F * [1 / (1+i)^n]                            *
      *                                                               *
      *       P = Present Value                                       *
      *       F = Future Value                                        *
      *       i = annual-interest (rate)                              *
      *       n = loan-term (years)                                   *
      *                                                               *
      *****************************************************************

       01 CALC-FIELDS.
              05 PRESENT-VALUE     PIC S9(9)V99 USAGE COMP.
              05 ANNUAL-INTEREST   PIC 99V9(4) USAGE COMP.
              05 ANNUAL-TERM       PIC 99 USAGE COMP.
              05 NUMERATOR         PIC S9(9)V9(6) USAGE COMP.
              05 DENOMINATOR       PIC S9(9)V9(6) USAGE COMP.
              05 FUTURE-VALUE      PIC S9(9)V99 USAGE COMP.

       01 DISP-FIELDS.
              05 PV-OUT            PIC $ZZ,ZZZ,ZZ9.99 USAGE DISPLAY.
              05 FV-OUT            PIC $ZZZ,ZZZ,ZZ9.99 USAGE DISPLAY.
              05 INTEREST-RATE     PIC Z9.99 USAGE DISPLAY.
              05 TERM-OUT          PIC Z9 USAGE DISPLAY.

       procedure division.
       init-ws.
              initialize calc-fields
              initialize disp-fields.

       user-input.
              display "PRESENT VALUE CALCULATOR"
              display "Enter zero for any parameter to end the program."
              display "Enter future value: "
              accept future-value
              if future-value = 0
                     go to end-program
              end-if
              if future-value > 999999999
                     display "Present value must be <= $999,999,999.99"
                     go to user-input
              end-if

              display "Enter annual interest rate as a % value: "
              accept annual-interest
              if annual-interest = 0
                     go to end-program
              end-if
              if annual-interest > 26
                     display "Interest must be <= 26%"
                     go to user-input
              end-if

              display "Enter term in years: "
              accept annual-term
              if annual-term = 0
                     go to end-program
              end-if
              if annual-term > 30
                     display "Term must be <= 30 years."
                     go to user-input
              end-if.

       calculate-it.

      *****************************************************************
      *                                                               *
      * Present Value formula:                                        *
      *                                                               *
      *              P = F * [1 / (1+i)^n]                            *
      *                                                               *
      *       P = Present Value                                       *
      *       F = Future Value                                        *
      *       i = annual-interest (rate)                              *
      *       n = loan-term (years)                                   *
      *                                                               *
      *****************************************************************

              move annual-term to term-out
              move future-value to fv-out
              move annual-interest to interest-rate

              divide 100 into annual-interest

              compute numerator = (1+annual-interest) **
              annual-term
              divide 1 by numerator giving denominator

              compute present-value = future-value *
              denominator

              move present-value to pv-out.

       disp-result.
              display "Future Value: " fv-out
              display "Term: " term-out " years"
              display "Interest Rate: " interest-rate "%"
              display "You need to invest: " pv-out.

       end-program.
              stop run.

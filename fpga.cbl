      *****************************************************************
      * FIND P GIVEN A                                                *
      *                                                               *
      * A simple program that calculates the Present Value of an      *
      * investment after the user enters the Uniform Series Amount,   *
      * the annual interest rate, and the term of the investment.     *
      *                                                               *
      *****************************************************************

       identification division.
       program-id.   fpga.

       data division.
       working-storage section.

      *****************************************************************
      *                                                               *
      * Present Value formula:                                        *
      *                                                               *
      *              P = A * [(1+i)^n -1 / i(1+i)^n]                  *
      *                                                               *
      *       P = Present Value                                       *
      *       A = Uniform Series Amount                               *
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
              05 FACTOR            PIC S9(9)V9(6) USAGE COMP.
              05 US-AMOUNT         PIC S9(9)V99 USAGE COMP.

       01 DISP-FIELDS.
              05 PV-OUT            PIC $ZZ,ZZZ,ZZ9.99 USAGE DISPLAY.
              05 AMOUNT-OUT        PIC $ZZZ,ZZZ,ZZ9.99 USAGE DISPLAY.
              05 INTEREST-RATE     PIC Z9.99 USAGE DISPLAY.
              05 TERM-OUT          PIC Z9 USAGE DISPLAY.

       procedure division.
       init-ws.
              initialize calc-fields
              initialize disp-fields.

       user-input.
              display "PRESENT VALUE CALCULATOR"
              display "Enter zero for any parameter to end the program."
              display "Enter value of uniform series amount: "
              accept us-amount
              if us-amount = 0
                     go to end-program
              end-if
              if us-amount > 999999999
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
      *              P = A * [(1+i)^n -1 / i(1+i)^n]                            *
      *                                                               *
      *       P = Present Value                                       *
      *       A = Uniform Series Amount                               *
      *       i = annual-interest (rate)                              *
      *       n = loan-term (years)                                   *
      *                                                               *
      *****************************************************************

              move annual-term to term-out
              move us-amount to amount-out
              move annual-interest to interest-rate

              divide 100 into annual-interest

              compute numerator = (1+annual-interest) **
              annual-term
              multiply annual-interest by numerator giving
              denominator
              subtract 1 from numerator
              divide numerator by denominator giving factor 

              multiply us-amount by factor giving present-value

              move present-value to pv-out.

       disp-result.
              display "Uniform Series Amount: " amount-out
              display "Term: " term-out " years"
              display "Interest Rate: " interest-rate "%"
              display "You need to invest: " pv-out.

       end-program.
              stop run.

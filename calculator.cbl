       IDENTIFICATION DIVISION.
       PROGRAM-ID. calc.

      * Programme permettant d’imiter une
      *calculatrice exécutant les opérations suivantes :
      * ● Additionner
      * ● Soustraire
      * ● Multiplier
      * ● Diviser
      * ● [Bonus] Puissance

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Ensemble des datas. Les views seront les valeurs affichées en
      * terminal.

       01  NBR-A       PIC S9(3)V9(2) VALUE 0.
       01  A-VIEW      PIC ZZ9.99.
       01  NBR-B       PIC S9(3)V9(2) VALUE 0.
       01  B-VIEW      PIC ZZ9.99.
       01  RESULT      PIC S9(6)V9(2) VALUE 0.
       01  R-VIEW      PIC ZZ9.99.
       01  EXITCODE    PIC x(1) VALUE SPACE.
       01  OPERATOR    PIC x(1) VALUE "+".
       01  CHOICE      PIC x(1) VALUE "+".
       01  BOUCLE      PIC 9(2) VALUE 0.
       01  CHECKLENGTH PIC 99 VALUE 0.

      *date time management
       01  WS-DATE.
           05  WS-DAY       PIC 99.
           05 FILLER        PIC X VALUE '/'. 
           05  WS-MONTH     PIC 99.
           05 FILLER        PIC X VALUE '/'. 
           05  WS-YEAR      PIC 9(4).
       01  DATE-STRING      PIC X(8).
       01  WS-TIME.
           05  WS-HOUR      PIC 99.
           05 FILLER        PIC X VALUE ':'. 
           05  WS-MINUTE    PIC 99.
           05 FILLER        PIC X VALUE ':'. 
           05  WS-SECOND    PIC 99.

       PROCEDURE DIVISION.
      
      * Menuing


           DISPLAY " ------------------------------------------------".
           DISPLAY "|              Calculatrice  COBOL               |".
           DISPLAY " ------------------------------------------------".


           DISPLAY "Entrez une première valeur, un opérateur puis".  
           DISPLAY "une seconde valeur".
           DISPLAY "Liste des operateurs :".
           DISPLAY "- Addition:       +".
           DISPLAY "- Soustraction:   -".
           DISPLAY "- Division:       /".
           DISPLAY "- Multiplication: x".
           DISPLAY "- Puissance:      p".
           DISPLAY " ------------------------------------------------".
           DISPLAY "Entrez votre calcul:"

      * Saisie de la valeur A, servant de base

           DISPLAY SPACE.          
           ACCEPT NBR-A.
           MOVE NBR-A TO A-VIEW.
   
      * Le Programme bouclera 20 fois. Il sera possible de sortir après
      * chaque calcul.

           PERFORM 8000-begin
           THRU 8000-end
           UNTIL  BOUCLE = 20
           stop run.

       8000-begin.
           ADD 1 TO BOUCLE.

      * Un operateur sur les 5 proposés
           ACCEPT OPERATOR.

      * Deuxieme valeur ref: 

           ACCEPT NBR-B.
           MOVE NBR-B TO B-VIEW.
           MOVE OPERATOR TO CHOICE.
      
      * Switch: selon opérateur, calcul différent
           EVALUATE CHOICE
               WHEN '+' 
                   PERFORM ADDITIONS
               WHEN '-'
                   PERFORM SOUSTRACTIONS
               WHEN 'x'
                   PERFORM MULTIPLICATIONS
               WHEN '/'
                  PERFORM DIVISIONS
               WHEN 'p'
                  PERFORM PUISSANCES
               WHEN 'end'
                  PERFORM BYEBYE 
               WHEN OTHER
                   DISPLAY "Operateur inexistant."
           END-EVALUATE.
           
      * affichage du resultat : Si positif valeur tronquée
      * si positif, valeur calculée  
           DISPLAY " ------------------------------------------------".

           IF RESULT >= 0
           DISPLAY A-VIEW SPACE OPERATOR SPACE B-VIEW
           SPACE "=" SPACE R-VIEW
           ELSE
           DISPLAY A-VIEW SPACE OPERATOR SPACE B-VIEW
           SPACE "=" SPACE RESULT
           END-IF.
           DISPLAY " ------------------------------------------------".
      * affichage date et heure
           ACCEPT DATE-STRING FROM DATE YYYYMMDD.
           MOVE DATE-STRING(1:4) TO WS-YEAR.
           MOVE DATE-STRING(5:2) TO WS-MONTH.
           MOVE  DATE-STRING(7:2) TO  WS-DAY.
           DISPLAY "Le calcul a été fait le:" SPACE WS-DATE.
         
           MOVE FUNCTION WHEN-COMPILED(9:2) TO WS-HOUR.
           MOVE FUNCTION WHEN-COMPILED(11:2) TO WS-MINUTE.
           MOVE FUNCTION WHEN-COMPILED(13:2) TO WS-SECOND.
           DISPLAY "Compilé à :" SPACE WS-TIME.

      * Si on ne souhaite pas continuer, ferme le programme
           DISPLAY " ------------------------------------------------".
           DISPLAY "Continuer? (y/n)".
           ACCEPT  EXITCODE.
           DISPLAY " ------------------------------------------------".
           IF EXITCODE NOT EQUAL "n"
      * Si l'on souhaite continuer avec le reustat,
      *  réatribue le resultat a nbr-A.

           PERFORM CONTINUEWITHRESULT
           ELSE
           STOP RUN
           END-IF.
           
       8000-end.
 
      * Paragraphes de méthodes 
       BYEBYE.
           STOP RUN.
       ADDITIONS.
           ADD NBR-A TO NBR-B GIVING RESULT.
           MOVE RESULT TO R-VIEW.
           EXIT.

       SOUSTRACTIONS.
           SUBTRACT NBR-B FROM NBR-A GIVING RESULT.
           EXIT.

       MULTIPLICATIONS.
           MULTIPLY NBR-A BY NBR-B GIVING RESULT.
           MOVE RESULT TO R-VIEW.
           EXIT.

       DIVISIONS.
             IF NBR-B NOT = 0
             DIVIDE NBR-A BY NBR-B GIVING RESULT
             MOVE RESULT TO R-VIEW
             ELSE
             DISPLAY "division par 0 impossible"
             END-IF.

       PUISSANCES.
             COMPUTE RESULT = NBR-A ** NBR-B
             MOVE RESULT TO R-VIEW.
           EXIT.
           
       CONTINUEWITHRESULT.
           DISPLAY "Continuer avec" SPACE R-VIEW "? (y/n)".
           ACCEPT  EXITCODE.
           IF EXITCODE NOT EQUAL "n"
            SET NBR-A TO RESULT
            DISPLAY NBR-A
           ELSE
            DISPLAY "Nouveau calcul :"
            ACCEPT NBR-A
           END-IF.
           EXIT.


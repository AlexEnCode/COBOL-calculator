      *=============================================================*
      * CALCULATRICE:                                               *
      *    L'objectif: Concevoir un programme permettant d'imiter le*
      *    fonctionnement d'une calculatrice.                       *
      *    Les étapes:                                              *        
      *    Je demande à l'utilisateur de saisir l'opération qu'il   *
      *    veut faire, puis les chiffres qu'il veut mettre en oeuvre*
      *    je teste les données reçues et boucle sur la saisie si ko*
      *    Je calcule le résultat                                   *
      *    J'affiche l'opération et le résultat                     *
      *    auteur : AlexEnCode                                      *
      *    Date création 08/04/2024                                 *
      *=============================================================*

      ***************************************************************
      *               identification et déclarations                *
      ***************************************************************         

       IDENTIFICATION DIVISION.
       PROGRAM-ID. calc.
       AUTHOR. AlexEnCode

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Ensemble des datas. Les views seront les valeurs affichées en
      * terminal.

       01  WS-NBR-A            PIC S9(3)V9(2) VALUE 0.
       01  WS-A-VIEW           PIC ZZ9.99.
       01  WS-NBR-B            PIC S9(3)V9(2) VALUE 0.
       01  WS-B-VIEW           PIC ZZ9.99.
       01  WS-RESULT           PIC S9(6)V9(2) VALUE 0.
       01  WS-R-VIEW           PIC ZZ9.99.
       01  WS-EXITCODE         PIC x(1)       VALUE SPACE.
       01  WS-OPERATOR         PIC x(1)       VALUE "+".
       01  WS-CHOICE           PIC x(1)       VALUE "+".
       01  WS-BOUCLING         PIC 9          VALUE 0.
       01  WS-FINISHED         PIC 9          VALUE 1.
       01  WS-CHECKLENGTH      PIC 99         VALUE 0.
       01  WS-NUMBER-ENTRY     PIC X(13)      VALUE "NOMBRE    :  ".
       01  ws-OPERATEUR-ENTRY  PIC X(13)      VALUE "OPERATEUR :  ".

      *date time management
       01  WS-DATE.
           05  WS-DAY       PIC 99.
           05 FILLER        PIC X          VALUE '/'. 
           05  WS-MONTH     PIC 99.
           05 FILLER        PIC X          VALUE '/'. 
           05  WS-YEAR      PIC 9(4).
       01  DATE-STRING      PIC X(8).

       01  WS-TIME.
           05  WS-HOUR      PIC 99.
           05 FILLER        PIC X          VALUE ':'. 
           05  WS-MINUTE    PIC 99.
           05 FILLER        PIC X          VALUE ':'. 
           05  WS-SECOND    PIC 99.

      **************************************************************
      * Exécution du programme                                      
      **************************************************************
       PROCEDURE DIVISION.
      
      *0000-MAIN-START.

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
           DISPLAY "Entrez votre calcul:".
           DISPLAY SPACE.

      * Saisie de la valeur A, servant de base

           DISPLAY WS-NUMBER-ENTRY WITH NO ADVANCING.          
           ACCEPT WS-NBR-A.
           MOVE WS-NBR-A TO WS-A-VIEW.
   
      * Le Programme bouclera 20 fois. Il sera possible de sortir 
      * après chaque calcul.

           PERFORM 1000-BEGIN
           THRU 1000-END
           UNTIL WS-BOUCLING = WS-FINISHED.
           STOP RUN.

       1000-begin.

      * Un operateur sur les 5 proposés
           DISPLAY WS-OPERATEUR-ENTRY WITH NO ADVANCING. 
           ACCEPT WS-OPERATOR.

      * Deuxieme valeur ref: 
           DISPLAY WS-NUMBER-ENTRY WITH NO ADVANCING. 
           ACCEPT WS-NBR-B.
           MOVE WS-NBR-B TO WS-B-VIEW.
           MOVE WS-OPERATOR TO WS-CHOICE.
      
      * Switch: selon opérateur, calcul différent
           EVALUATE WS-CHOICE
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
               WHEN 'END'
                  PERFORM BYEBYE 
               WHEN OTHER
                   DISPLAY "Operateur inexistant."
           END-EVALUATE.
           
      * affichage du resultat : Si positif valeur tronquée
      * si positif, valeur calculée  
           DISPLAY " ------------------------------------------------".

           IF WS-RESULT >= 0
           DISPLAY WS-A-VIEW SPACE WS-OPERATOR SPACE WS-B-VIEW
           SPACE "=" SPACE WS-R-VIEW
           ELSE
           DISPLAY WS-A-VIEW SPACE WS-OPERATOR SPACE WS-B-VIEW
           SPACE "=" SPACE WS-RESULT
           END-IF.
           DISPLAY " ------------------------------------------------".

       CALCULTIMING.

      * Si on ne souhaite pas continuer, ferme le programme
           DISPLAY " ------------------------------------------------".
           DISPLAY "Continuer? (Y/N)".
           ACCEPT  WS-EXITCODE.
           MOVE FUNCTION UPPER-CASE (WS-EXITCODE) TO WS-EXITCODE.
           DISPLAY " ------------------------------------------------".
           IF WS-EXITCODE NOT EQUAL "N"
      * Si l'on souhaite continuer avec le reustat,
      *  réatribue le resultat a nbr-A.

           PERFORM CONTINUEWITHRESULT
           ELSE
           SET WS-BOUCLING TO WS-FINISHED
           STOP RUN
           END-IF.
           
       1000-END.
 
      *=============================================================*

      ***************************************************************
      *                  Paragraphes de méthodes                    *
      ***************************************************************

       BYEBYE.
           STOP RUN.
       ADDITIONS.
           ADD WS-NBR-A TO WS-NBR-B GIVING WS-RESULT.
           MOVE WS-RESULT TO WS-R-VIEW.
           EXIT.

       SOUSTRACTIONS.
           SUBTRACT WS-NBR-B FROM WS-NBR-A GIVING WS-RESULT.
           EXIT.

       MULTIPLICATIONS.
           MULTIPLY WS-NBR-A BY WS-NBR-B GIVING WS-RESULT.
           MOVE WS-RESULT TO WS-R-VIEW.
           EXIT.

       DIVISIONS.
             IF WS-NBR-B NOT = 0
             DIVIDE WS-NBR-A BY WS-NBR-B GIVING WS-RESULT
             MOVE WS-RESULT TO WS-R-VIEW
             ELSE
             DISPLAY "division par 0 impossible"
             END-IF.
             EXIT.

       PUISSANCES.
             COMPUTE WS-RESULT = WS-NBR-A ** WS-NBR-B
             MOVE WS-RESULT TO WS-R-VIEW.
           EXIT.
           
       CONTINUEWITHRESULT.
           DISPLAY "Continuer avec" SPACE WS-R-VIEW "? (y/n)".
           ACCEPT  WS-EXITCODE.
           MOVE FUNCTION UPPER-CASE (WS-EXITCODE) TO WS-EXITCODE
           IF WS-EXITCODE NOT EQUAL "N"
            SET WS-NBR-A TO WS-RESULT
            DISPLAY WS-NBR-A
           ELSE
            DISPLAY "Nouveau calcul :"
            ACCEPT WS-NBR-A
           END-IF.
           EXIT.

       CALCULTIMING.
           ACCEPT DATE-STRING FROM DATE YYYYMMDD.
           MOVE DATE-STRING(1:4) TO WS-YEAR.
           MOVE DATE-STRING(5:2) TO WS-MONTH.
           MOVE  DATE-STRING(7:2) TO  WS-DAY.
           DISPLAY "Le calcul a été fait le:" SPACE WS-DATE.
         
           MOVE FUNCTION WHEN-COMPILED(9:2) TO WS-HOUR.
           MOVE FUNCTION WHEN-COMPILED(11:2) TO WS-MINUTE.
           MOVE FUNCTION WHEN-COMPILED(13:2) TO WS-SECOND.
           DISPLAY "Compilé à :" SPACE WS-TIME.    
    
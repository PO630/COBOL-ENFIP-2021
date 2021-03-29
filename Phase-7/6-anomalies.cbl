      ******************************************************************
      * Author: Kevin Ropital
      * Date: 25/03/2021
      * Purpose: PROJET COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 6-ANOMALIES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FE ASSIGN TO "6-anomalies.dat"
                   ORGANIZATION LINE SEQUENTIAL.

           SELECT FS ASSIGN TO "7-anomalies.txt"
                   ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  FE.
       01  FE-DATA.
           05 E-Identite.
               10 E-Numero-Fiscal PIC 9(8).
               10 E-Prenom PIC X(15).
               10 E-Nom PIC X(11).
               10 E-Code-Insee PIC 9(5).
           05 E-Lieu.
               10 E-Commune PIC X(50).
                   88 E-Commune-Nulle VALUE SPACE.
               10 E-Departement PIC X(28).
               10 E-Region PIC X(30).
           05 E-Occurence.
               10 E-Taxe PIC X(2).
               10 E-Revenu PIC 9(6).
           05 E-Erreur PIC X(50).

       FD  FS.
       01  FS-DATA PIC X(112).

       WORKING-STORAGE SECTION.

       01  WSS-DATA.
           05 CPT PIC 99.

       01  Affichage.

           05 ARTICLE-INDIV.
               10 COLONNE-NUM.
                   15 FILLER PIC X.
                   15 Contribuable PIC 9(8).
                   15 FILLER PIC X.
               10 COLONNE-INDIV.
                   15 Nom-Prenom PIC X(20).
                   15 FILLER PIC X VALUE '|'.
               10 COLONNE-LIEU.
                   15 Lieu PIC X(30).
                   15 FILLER PIC X VALUE '|'.
               10 COLONNE-TAXE.
                   15 Taxe PIC X(6).
                   15 FILLER PIC X VALUE '|'.
               10 COLONNE-REVENU.
                   15 Revenu PIC ZZZBZZ9.99.
                   15 FILLER PIC X VALUE '|'.
               10 COLONNE-ERREUR.
                   15 Erreur PIC X(31).
                   15 FILLER PIC X VALUE '|'.

           05 ARTICLE-TITRE PIC X(30) VALUE 'Liste des anomalies :'.

           05 ARTICLE-LIGNE.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(8) VALUE '--------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(20) VALUE '--------------------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(30)
                  VALUE '------------------------------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(6) VALUE '------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(10) VALUE '----------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(31)
               VALUE '-------------------------------'.
               10 FILLER PIC X VALUE '+'.

           05 ARTICLE-ENTETE.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(8) VALUE 'Contrib.'.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(20) VALUE 'Nom Prenom '.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(30) VALUE 'Lieu'.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(6) VALUE 'Taxe'.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(10) VALUE 'Revenu'.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(31) VALUE 'Erreur'.
               10 FILLER PIC X VALUE '|'.

           05 ARTICLE-FIN.
               10 FILLER PIC X(16) VALUE 'Nombre'.
               10 FILLER PIC X VALUE ':'.
               10 Nombre-fin PIC Z(11)9.

       01  EOF-MANAGER.
           05 FIN-ENREG PIC  X(01) VALUE SPACE.
               88 FF VALUE HIGH-VALUE.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT FE
           OPEN OUTPUT FS

           MOVE ARTICLE-TITRE TO FS-DATA
           WRITE FS-DATA
           MOVE ARTICLE-LIGNE TO FS-DATA
           WRITE FS-DATA
           MOVE ARTICLE-ENTETE TO FS-DATA
           WRITE FS-DATA
           MOVE ARTICLE-LIGNE TO FS-DATA
           WRITE FS-DATA

           PERFORM UNTIL FF
               READ FE
                   AT END
                       SET FF TO TRUE
                   NOT AT END
                   INITIALIZE Nom-Prenom
                   INITIALIZE Lieu
                       ADD 1 TO CPT
                       STRING '|' E-Numero-Fiscal '|'
                       INTO COLONNE-NUM
                       STRING FUNCTION TRIM(E-Nom) DELIMITED BY SIZE
                              ', ' DELIMITED BY SIZE
                              E-Prenom DELIMITED BY SPACE
                       INTO Nom-Prenom
                       IF E-Commune-Nulle THEN
                           MOVE SPACE TO Lieu
                       ELSE
                           STRING FUNCTION TRIM(E-Commune)
                                  DELIMITED BY SIZE
                                  ', ' DELIMITED BY SIZE
                                  E-Departement DELIMITED BY SPACE
                                  ', ' DELIMITED BY SIZE
                                  E-Region DELIMITED BY SPACE
                           INTO Lieu
                       END-IF
                       MOVE E-Taxe TO Taxe
                       MOVE E-Revenu TO Revenu
                       MOVE E-Erreur TO Erreur
                       MOVE ARTICLE-INDIV TO FS-DATA
                       WRITE FS-DATA
               END-READ
           END-PERFORM

           MOVE ARTICLE-LIGNE TO FS-DATA
           WRITE FS-DATA
           MOVE CPT TO Nombre-fin
           MOVE ARTICLE-FIN TO FS-DATA
           WRITE FS-DATA

           CLOSE FE FS
           .


       FIN-PGM.
           STOP RUN.

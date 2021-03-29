      ******************************************************************
      * Author: Kevin Ropital
      * Date: 25/03/2021
      * Purpose: PROJET COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 6-IMPOSABLES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FE ASSIGN TO "6-imposables.dat"
                   ORGANIZATION LINE SEQUENTIAL.

           SELECT FS ASSIGN TO "7-imposables.txt"
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
               10 E-Departement PIC X(28).
               10 E-Region PIC X(30).
           05 E-Imposition.
               10 E-Impot PIC 9(6).
               10 E-Impot-Commune PIC 9(6).
               10 E-Impot-Dept PIC 9(6).
               10 E-Impot-Region PIC 9(6).
               10 E-Occurences PIC 9(2).

       FD  FS.
       01  FS-DATA PIC X(121).

       WORKING-STORAGE SECTION.

       01  WSS-DATA.
           05 CPT PIC 9999.
           05 WSS-TOTAL PIC 9(10)V99.
           05 WSS-TOTAL-C PIC 9(10)V99.
           05 WSS-TOTAL-D PIC 9(10)V99.
           05 WSS-TOTAL-R PIC 9(10)V99.

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
               10 COLONNES-VALUES.
                   15 Valeur-tot PIC ZBZZZBZZ9.99.
                   15 FILLER PIC X VALUE '|'.
                   15 Valeur-c PIC ZBZZZBZZ9.99.
                   15 FILLER PIC X VALUE '|'.
                   15 Valeur-d PIC ZBZZZBZZ9.99.
                   15 FILLER PIC X VALUE '|'.
                   15 Valeur-r PIC ZBZZZBZZ9.99.
                   15 FILLER PIC X VALUE '|'.
                   15 Occurences PIC Z(5)9.
                   15 FILLER PIC X VALUE '|'.

           05 ARTICLE-TITRE PIC X(30) VALUE 'Liste des imposables :'.

           05 ARTICLE-LIGNE.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(8) VALUE '--------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(20) VALUE '--------------------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(30)
                  VALUE '------------------------------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(12) VALUE '------------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(12) VALUE '------------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(12) VALUE '------------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(12) VALUE '------------'.
               10 FILLER PIC X VALUE '+'.
               10 FILLER PIC X(6) VALUE '------'.
               10 FILLER PIC X VALUE '+'.

           05 ARTICLE-ENTETE.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(8) VALUE 'Contrib.'.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(20) VALUE 'Nom Prenom '.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(30) VALUE 'Lieu'.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(12) VALUE 'Total'.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(12) VALUE 'Commune'.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(12) VALUE 'Dept.'.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(12) VALUE 'Region'.
               10 FILLER PIC X VALUE '|'.
               10 FILLER PIC X(6) VALUE 'Occs'.
               10 FILLER PIC X VALUE '|'.

           05 ARTICLE-FIN.
               10 Nombre.
                   15 FILLER PIC X(16) VALUE 'Nombre'.
                   15 FILLER PIC X VALUE ':'.
                   15 Nombre-fin PIC Z(11)9.
               10 Total.
                   15 FILLER PIC X(16) VALUE 'Total'.
                   15 FILLER PIC X VALUE ':'.
                   15 Total-fin PIC Z(11)9.99.
               10 Communes.
                   15 FILLER PIC X(16) VALUE 'Communes'.
                   15 FILLER PIC X VALUE ':'.
                   15 Communes-fin PIC Z(11)9.99.
               10 Departements.
                   15 FILLER PIC X(16) VALUE 'Departements'.
                   15 FILLER PIC X VALUE ':'.
                   15 Departements-fin PIC Z(11)9.99.
               10 Regions.
                   15 FILLER PIC X(16) VALUE 'Regions'.
                   15 FILLER PIC X VALUE ':'.
                   15 Regions-fin PIC Z(11)9.99.

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
                       COMPUTE WSS-TOTAL =
                               WSS-TOTAL + E-Impot
                       COMPUTE WSS-TOTAL-C =
                               WSS-TOTAL-C + E-Impot-Commune
                       COMPUTE WSS-TOTAL-D =
                               WSS-TOTAL-D + E-Impot-Dept
                       COMPUTE WSS-TOTAL-R =
                               WSS-TOTAL-R + E-Impot-Region
                       STRING '|' E-Numero-Fiscal '|'
                       INTO COLONNE-NUM
                       STRING FUNCTION TRIM(E-Nom) DELIMITED BY SIZE
                              ', ' DELIMITED BY SIZE
                              E-Prenom DELIMITED BY SPACE
                       INTO Nom-Prenom
                       STRING FUNCTION TRIM(E-Commune) DELIMITED BY SIZE
                              ', ' DELIMITED BY SIZE
                              E-Departement DELIMITED BY SPACE
                              ', ' DELIMITED BY SIZE
                              E-Region DELIMITED BY SPACE
                       INTO Lieu
                       MOVE E-Impot TO Valeur-tot
                       MOVE E-Impot-Commune TO Valeur-c
                       MOVE E-Impot-Dept TO Valeur-d
                       MOVE E-Impot-Region TO Valeur-r
                       MOVE E-Occurences TO Occurences
                       MOVE ARTICLE-INDIV TO FS-DATA
                       WRITE FS-DATA
               END-READ
           END-PERFORM

           MOVE ARTICLE-LIGNE TO FS-DATA
           WRITE FS-DATA
           MOVE CPT TO Nombre-fin
           MOVE Nombre TO FS-DATA
           WRITE FS-DATA
           MOVE WSS-TOTAL TO Total-fin
           MOVE Total TO FS-DATA
           WRITE FS-DATA
           MOVE WSS-TOTAL-C TO Communes-fin
           MOVE Communes TO FS-DATA
           WRITE FS-DATA
           MOVE WSS-TOTAL-D TO Departements-fin
           MOVE Departements TO FS-DATA
           WRITE FS-DATA
           MOVE WSS-TOTAL-R TO Regions-fin
           MOVE Regions TO FS-DATA
           WRITE FS-DATA

           CLOSE FE FS
           .

       FIN-PGM.
            STOP RUN.

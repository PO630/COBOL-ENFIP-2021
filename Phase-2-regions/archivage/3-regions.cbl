      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 3-regions.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INPUT-FILE ASSIGN TO "4-regions.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS1.

           SELECT OUTPUT-FILE ASSIGN TO "3-regions.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS2.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD INPUT-FILE.
       01 INP-REC.
           05 I-COMMUNE   PIC 9(5).
           05 I-COMM-DESC   PIC X(50).
           05 I-DEPT        PIC X(28).
           05 I-REGION      PIC X(30).
           05 I-ALTITUDE    PIC 9(6)V99.
           05 I-SUPERFICIE  PIC 9(6)V99.
           05 I-POPULATION  PIC 9(6)V99.
           05 FILLER        PIC 9(4)V99.
           05 FILLER        PIC 9(4)V99.

       FD OUTPUT-FILE.
       01 OUT-REC.
           05 O-TOT-COMM    PIC 9(6).
           05 O-REGION      PIC X(30).
           05 O-AVG-ALT     PIC 9(6)V99.
           05 O-AVG-SUPER   PIC 9(6)V99.
           05 O-AVG-POP     PIC 9(6)V99.
           05 O-TAUX-CARBON PIC 9(4)V99.
           05 O-TAUX-DENS   PIC 9(4)V99.

           
       WORKING-STORAGE SECTION.
       77 FS1     PIC 9(02).
       77 FS2     PIC 9(02).
       01 W-INP-REC.
           05 W-COMMUNE   PIC 9(6).
           05 W-REGION      PIC X(30).
           05 W-ALTITUDE    PIC 9(6)V99.
           05 W-SUPERFICIE  PIC 9(6)V99.
           05 W-POPULATION  PIC 9(6)V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM INIT-PARA.
            PERFORM OPEN-PARA.
            PERFORM PROCESS-PARA UNTIL FS1 NOT = 0.
            PERFORM CLOSE-PARA.
            STOP RUN.
       INIT-PARA.
            INITIALIZE W-INP-REC.

       OPEN-PARA.
            OPEN INPUT INPUT-FILE.
            IF FS1 NOT = 0
                DISPLAY "FICHIER NON TROUVE"
                STOP RUN
            END-IF.
            OPEN OUTPUT OUTPUT-FILE.

       PROCESS-PARA.
            READ INPUT-FILE.
            IF FS1 = 0
                IF W-REGION = I-REGION OR
                    W-REGION = SPACES
                    PERFORM MOVE-INP-TO-WRK
                ELSE
                    PERFORM MOVE-WRK-TO-OUT
                    PERFORM MOVE-INP-TO-WRK
                END-IF
           ELSE IF FS1 = 10
               PERFORM MOVE-WRK-TO-OUT
           ELSE
               DISPLAY "ERREUR DE LECTURE"
               DISPLAY FS1
               STOP RUN
           END-IF.

       MOVE-INP-TO-WRK.
           ADD  1            TO W-COMMUNE.
           MOVE I-REGION     TO W-REGION.
           ADD  I-ALTITUDE   TO W-ALTITUDE.
           ADD  I-SUPERFICIE TO W-SUPERFICIE.
           ADD  I-POPULATION TO W-POPULATION.

       MOVE-WRK-TO-OUT.
           MOVE W-COMMUNE TO O-TOT-COMM.
           MOVE W-REGION    TO O-REGION.
           COMPUTE O-AVG-ALT = W-ALTITUDE / W-COMMUNE.
           COMPUTE O-AVG-SUPER = W-SUPERFICIE / W-COMMUNE.
           COMPUTE O-AVG-POP = (W-POPULATION / W-COMMUNE) / 1000.
           COMPUTE O-TAUX-CARBON = (3000 - O-AVG-ALT) * 0.0005.
           COMPUTE O-TAUX-DENS = 1 + (((O-AVG-POP * 1000) /
                                 O-AVG-SUPER) / 100).
           WRITE OUT-REC.
           MOVE 0 TO W-COMMUNE.
           IF FS2 NOT = 0
               DISPLAY "ERREUR DE LECTURE"
               STOP RUN
           END-IF.

       CLOSE-PARA.
           CLOSE INPUT-FILE OUTPUT-FILE.

       END PROGRAM 3-regions.

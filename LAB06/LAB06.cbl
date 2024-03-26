      ******************************************************************
      * Author: Mostapha A
      * Purpose: Record car details from an external file and load a 
      * 		 table sequentially with the records read
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. LAB06.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT CAR-FILE
              ASSIGN TO "../CARFILE.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD CAR-FILE.
       01 CAR-RECORD.
           05 CAR-TYPE     PIC X(5).
           05 CAR-YEAR     PIC 9(4).
           05 ENGINE-SIZE  PIC 9(1).

       WORKING-STORAGE SECTION.
       01 CONTROL-FIELDS.
           05 SUB-1 PIC 9(3).
           05 EOF-FLAG PIC A(1).
           05 TABLE-SIZE PIC 9(3).

       01 CAR-DATA.
           05 CAR-TABLE OCCURS 200 TIMES.
               10 FILLER PIC X VALUE SPACE.
               10 CAR-TYPE-CLM PIC X(5).
               10 FILLER PIC X(3) VALUE " | ".
               10 CAR-YEAR-CLM PIC 9(4).
               10 FILLER PIC X(7) VALUE "  |    ".
               10 ENGINE-SIZE-CLM PIC 9.
               10 FILLER PIC X(3) VALUE SPACE.

       01 CAR-TABLE-HEADER.
           05 CAR-TYPE-TITLE PIC X(9) VALUE " MAKE  | ".
           05 CAR-YEAR-TITLE PIC X(9) VALUE "YEAR  | ".
           05 CAR-SIZE-TITLE PIC X(6) VALUE "LITRES".

       01 CAR-TABLE-LINE.
           05 FILLER PIC X(24) VALUE ALL "-".

       PROCEDURE DIVISION.
       100-CREATE-CAR-TABLE.
           PERFORM 201-INITIALIZE.
           PERFORM 202-LOAD-CAR-TABLE
               VARYING SUB-1
               FROM 1
               BY 1
               UNTIL SUB-1 > 200 OR EOF-FLAG = "Y".
           PERFORM 204-DISPAY-HEADER.

           MOVE 1 TO SUB-1.

           PERFORM 205-DISPLAY-TABLE
                TABLE-SIZE TIMES.
      *         VARYING SUB-1
      *         FROM 1
      *         BY 1
      *         UNTIL (CAR-TYPE-CLM(SUB-1) = SPACE OR LOW-VALUE)
      *         AND CAR-YEAR-CLM(SUB-1) = 0.
           PERFORM 203-CLOSE-FILE.
           STOP RUN.

       201-INITIALIZE.
           PERFORM 301-OPEN-CAR-FILE.
           PERFORM 302-READ-CAR-FILE.

       202-LOAD-CAR-TABLE.
           ADD 1 TO TABLE-SIZE.
           PERFORM 303-MOVE-ONE-CAR-RECORD.
           PERFORM 302-READ-CAR-FILE.

       203-CLOSE-FILE.
           CLOSE CAR-FILE.

       301-OPEN-CAR-FILE.
           OPEN INPUT CAR-FILE.

       302-READ-CAR-FILE.
           READ CAR-FILE AT END MOVE "Y" TO EOF-FLAG.

       303-MOVE-ONE-CAR-RECORD.
           MOVE CAR-TYPE TO CAR-TYPE-CLM(SUB-1).
           MOVE CAR-YEAR TO CAR-YEAR-CLM(SUB-1).
           MOVE ENGINE-SIZE TO ENGINE-SIZE-CLM(SUB-1).

       204-DISPAY-HEADER.
           DISPLAY CAR-TABLE-HEADER.
           DISPLAY CAR-TABLE-LINE.

       205-DISPLAY-TABLE.
           DISPLAY CAR-TABLE(SUB-1).
           ADD 1 TO SUB-1.

       END PROGRAM LAB06.

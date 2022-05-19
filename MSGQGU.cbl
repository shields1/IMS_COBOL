       IDENTIFICATION DIVISION.
       PROGRAM-ID.     MSGQGU.
      ******************************************************************
      *
      *    MODULE      MSGQGU
      *                MPP
      *                SAMPLE IMS COBOL APPLICATION FOR A
      *                GET UNIQUE CALL TO THE IMS MESSAGE QUEUE
      *                UNTIL NO MORE MESSAGES ON QUEUE
      *                (RETURN CODE 'QC')
      *
      *    CODED       2022-05-06
      *
      *    CODED BY    ISAK SHIELDS
      *                
      *
      ******************************************************************
      ******************************************************************
      *
      *    LIST OF AMENDMENTS
      *    DATE     BY       THE CHANGE REFERS
      *
      *    000000   XXXXXX   YYYYYYYYYYYYYYY
      *
      *
      ******************************************************************
      ******************************************************************
      *
      *    ROUTINE LIST
      *
      *    A     = MAINROUTINE
      *    B     = INITIATION
      *    C     = GET UNIQUE MESSAGE FROM IMS QUEUE
      *    D     = SEND RESPONSE TO USER
      *    X     = ERROR HANDLING
      *    Z     = EXIT
      *
      ******************************************************************
      *
      *    MODULES CALLED
      *
      *    AIBTDLI - IMS APPLICATION INTERFACE
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      ******************************************************************
      *    WORKING STORAGE
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  FILLER                   PIC X(32)   VALUE
                                    'XXX MODULE MSGQGU START WSS XXX'.
       01  WORKAREAS.
      ******************************************************************
      *    IMS IO AREAS
      ******************************************************************
           05  INTRANS.
                10 LL-IN                    PIC S9(3) COMP.
                10 ZZ-IN                    PIC S9(3) COMP.
                10 TRANCODE                 PIC X(08).
                10 INDATA                   PIC X(20).
           05  IMSRESPONSE.
                10 LL-SVAR                  PIC S9(3) COMP.
                10 ZZ-SVAR                  PIC S9(3) COMP.
                10 UTDATA1                  PIC X(44)
                   VALUE ' >HELLO FROM MSGQGU! TRANSACTION INPUT WAS: '.
                10 UTDATA2                  PIC X(20).
      ******************************************************************
      *    MESSAGEAREA
      ******************************************************************
         05  MESSAGEAREA.
           10  MODULEDESCRIPTION            PIC X(32).
           10  MODULEAREA.
             15  MODULENAME                 PIC X(08).
      ******************************************************************
      *    CONSTANTS (CC)
      ******************************************************************
       01  CONSTANTS.
           05  CC-DLIKODER.
                10 CC-GU                    PIC X(04)  VALUE 'GU'.
                10 CC-OK                    PIC X(02)  VALUE 'OK'.
                10 CC-IOPCB                 PIC X(08)  VALUE 'IOPCB'.
                10 CC-ISRT                  PIC X(04)  VALUE 'ISRT'.
                10 CC-QC                    PIC X(04)  VALUE 'QC  '.
      ******************************************************************
      *    SWITCHES (SW)
      ******************************************************************
       01  SWITCHES.
           05 SW-INDICATOR                  PIC X      VALUE '0'.
             88 SW-ALL-OK                              VALUE '0'.
             88 SW-NO-MORE-MESSAGES                    VALUE 'S'.
             88 SW-INTERRUPTION                        VALUE 'F'.
      ******************************************************************
      *    APPLICATION INTERFACE BLOCK (AIB)
      ******************************************************************
       01 AIB.
         05 AIB-ID                  PIC X(08)          VALUE 'DFSAIB'.
         05 AIB-LEN                 PIC 9(09)   COMP   VALUE 128.
         05 AIB-SUB-FUNC.
           10 AIB-SUB-FUNC-1        PIC X(04)          VALUE SPACE.
           10 AIB-SUB-FUNC-2        PIC X(04)          VALUE SPACE.
         05 AIB-PCB-NAME            PIC X(08)          VALUE SPACE.
         05 FILLER                  PIC X(16)          VALUE SPACE.
         05 AIB-IOAREA-LENGTH       PIC 9(09)   COMP   VALUE ZERO.
         05 AIB-IOAREA-USED         PIC 9(09)   COMP.
         05 FILLER                  PIC X(12)          VALUE SPACE.
         05 AIB-RETURN-CODE         PIC 9(09)   COMP.
         05 AIB-REASON-CODE         PIC 9(09)   COMP.
         05 FILLER                  PIC X(04)          VALUE SPACE.
         05 AIB-PCB-PTR             POINTER.
         05 FILLER                  PIC X(48)          VALUE SPACE.
      ******************************************************************
      *    AIB RETURNCODES
      ******************************************************************
       01 AIB-RETURKODER.
          05 AIB-HEX-0000               PIC 9(9) COMP VALUE 0.
          05 AIB-HEX-000C               PIC 9(9) COMP VALUE 12.
          05 AIB-HEX-0100               PIC 9(9) COMP VALUE 256.
          05 AIB-HEX-0104               PIC 9(9) COMP VALUE 260.
          05 AIB-HEX-0208               PIC 9(9) COMP VALUE 520.
          05 AIB-HEX-0210               PIC 9(9) COMP VALUE 528.
          05 AIB-HEX-0214               PIC 9(9) COMP VALUE 532.
          05 AIB-HEX-0218               PIC 9(9) COMP VALUE 536.
          05 AIB-HEX-0900               PIC 9(9) COMP VALUE 2304.
      *
       01  FILLER                   PIC X(30)   VALUE
                                    'XXX MODULE MSGQGU END WSS XXXX'.
      ******************************************************************
      *
      *    LINKAGE SECTION
      *
      ******************************************************************
       LINKAGE SECTION.
        01  IOPCB.
           05 IO-LTERM            PIC X(8).
           05 IO-RESERVED         PIC X(2).
           05 IO-STATUS-CODE      PIC X(2).
           05 IO-DATE-YYDDD       PIC S9(7)   COMP-3.
           05 IO-TIME-HHMMSS-T    PIC S9(6)V9 COMP-3.
           05 IO-MSG-NO           PIC S9(9)   COMP.
           05 IO-MOD-NAME         PIC X(8).
           05 IO-USERID           PIC X(8).
      ******************************************************************
      *
      *    PROCEDURE DIVISION
      *
      ******************************************************************
       PROCEDURE DIVISION.
       A-MAINSECTION SECTION.
      
           PERFORM B-INITIATE-NOTIAREA
      
           PERFORM C-GET-IMS-MESSAGE
      
           PERFORM Z-EXIT
      
           .
      ******************************************************************
      *
      *    B   INITIATE MESSAGEAREA
      *
      ******************************************************************
      
       B-INITIATE-NOTIAREA SECTION.
           MOVE 'PERFORM GU CALL AGAINST IMS QUEUE' TO MODULEDESCRIPTION
           MOVE 'MSGQGU'                            TO MODULENAME
      
           CONTINUE.
      
      ******************************************************************
      *
      *    C   GET UNIQUE MESSAGE FROM IMS QUEUE
      *
      ******************************************************************
       C-GET-IMS-MESSAGE SECTION.
           MOVE 'IOPCB'                      TO AIB-PCB-NAME
           MOVE LENGTH OF INTRANS            TO AIB-IOAREA-LENGTH
           PERFORM UNTIL SW-NO-MORE-MESSAGES OR SW-INTERRUPTION
               MOVE SPACES                   TO INDATA
      
               CALL 'AIBTDLI' USING CC-GU
                                    AIB
                                    INTRANS
               END-CALL
               PERFORM X-AIB-KONTROLL
               MOVE INDATA                   TO UTDATA2
      
               PERFORM D-SEND-IMS-RESPONSE
           END-PERFORM
           CONTINUE.
      ******************************************************************
      *
      *    D   SEND RESPONSE TO USER
      *
      ******************************************************************
       D-SEND-IMS-RESPONSE SECTION.
           MOVE LENGTH OF IMSRESPONSE        TO LL-SVAR
      
           CALL 'AIBTDLI' USING CC-ISRT
                                AIB
                                IMSRESPONSE
           END-CALL
           PERFORM X-AIB-KONTROLL
           CONTINUE.
      ******************************************************************
      *
      *    X   AIB RETURNCODE CHECK
      *
      ******************************************************************
       X-AIB-KONTROLL.
               EVALUATE TRUE
                   WHEN AIB-RETURN-CODE = AIB-HEX-0000 AND
                        AIB-REASON-CODE = AIB-HEX-0000
                     CONTINUE
                   WHEN AIB-RETURN-CODE = AIB-HEX-0900
                     SET ADDRESS OF IOPCB TO AIB-PCB-PTR
                     IF IO-STATUS-CODE = 'QC'
                        SET SW-NO-MORE-MESSAGES TO TRUE
                     ELSE
                        SET SW-INTERRUPTION TO TRUE
                     END-IF
                   WHEN OTHER
                     SET ADDRESS OF IOPCB TO AIB-PCB-PTR
                     SET SW-INTERRUPTION TO TRUE
               END-EVALUATE
           CONTINUE.
      ******************************************************************
      *
      *    Z   EXIT
      *
      ******************************************************************
       Z-EXIT SECTION.
           IF SW-ALL-OK
              CONTINUE
           ELSE
              DISPLAY '**************************'
              DISPLAY '     MSGQGU AVSLUTAR      '
              DISPLAY 'RUN STATUS______________: INTERRUPTION'
              DISPLAY 'MODULENAMN______________: ' MODULENAME
              DISPLAY 'MODULEDESCRIPTION_______: ' MODULEDESCRIPTION
              DISPLAY 'PCB-STATUS-CODE_________: ' IO-STATUS-CODE
              DISPLAY 'AIB-RETURN-CODE_________: ' AIB-RETURN-CODE
              DISPLAY 'AIB-REASON-CODE_________: ' AIB-REASON-CODE
              DISPLAY '**************************'
           END-IF
      *
           GOBACK.
      *
      

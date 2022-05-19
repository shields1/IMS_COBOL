       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID. IMSMQ01.
      ******************************************************************
      *
      *    MODULE      IMSMQ01
      *                MPP
      *                READS MESSAGE FROM MQ QUEUE OR
      *                WRITES MESSAGE TO MQ QUEUE BASED ON INPUT FROM
      *                USER.
      *
      *    INPUT       xxxxxxxxyyyyyzzzzzaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      *                |       |     |      |
      *                > TRANS |     |      |
      *                        >READQ|      |
      *                         WRITE|      |
      *                              >QUANT |
      *                                     >MQ QUEUE
      *    CODED       2022-05-16
      *
      *    CODED BY    ISAK SHIELDS
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
      *    C     = TRANSACTION PROCESSING
      *    D
      *     DA   = CONNECT TO QUEUE MANAGER
      *     DB   = OPEN EXISTING MQ QUEUE
      *     DC   = GET MQ MESSAGE
      *     DD   = PUT MQ MESSAGE
      *     DE   = CLOSE MQ QUEUE
      *     DF   = DISCONNECT FROM QUEUE MANAGER
      *     DG   = SEND RESPONSE TO USER
      *    X     = AIB RETURNCODE CHECK
      *    Z     = EXIT
      *
      ******************************************************************
      *
      *    MODULER SOM ANROPAS
      *
      *    AIBTDLI - UTFÖR ANROP MOT IMS
      *    MQCONN  - CONNECT TO QUEUE MANAGER
      *    MQOPEN  - OPEN EXISTING MQ QUEUE
      *    MQGET   - GET MQ MESSAGE
      *    MQPUT   - PUT MQ MESSAGE
      *    MQCLOSE - CLOSE MQ QUEUE
      *    MQDISC  - DISCONNECT FROM QUEUE MANAGER
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
      *---------------------
       DATA DIVISION.
      *--------------
       WORKING-STORAGE SECTION.
       01  FILLER                        PIC X(32)       VALUE
                                       'X MODULE IMSMQ01 START WSS X'.
      ******************************************************************
      *    IN AREA
      ******************************************************************
       01  INTRANS.
           10 LL                         PIC S9(3) COMP.
           10 ZZ                         PIC S9(3) COMP.
           10 TRAN-CODE                  PIC X(8).
           10 INDATA.
              15 INDATA1                 PIC X(05).
              15 INDATA2                 PIC 9(05).
              15 INDATA3                 PIC X(30).
      *     10 FILLER                     PIC X(2000).
      ******************************************************************
      *    RESPONSE AREA
      ******************************************************************
       01  RESPONSE.
           10 RESPONSELL                 PIC S9(3) COMP.
           10 RESPONSEZZ                 PIC S9(3) COMP.
           10 OUTDATA                    PIC X(20).
           05 WS-DATE.
            10  WS-YEAR                  PIC 9(02).
            10  WS-MONTH                 PIC 9(02).
            10  WS-DAY                   PIC 9(02).
           05 WS-TIME.
            10  WS-TIM                   PIC 9(02).
            10  WS-MIN                   PIC 9(02).
            10  WS-SEC                   PIC 9(02).
            10  WS-HUNDRED               PIC 9(02).
      ******************************************************************
      *    IMS DLICALLS
      ******************************************************************
       01  KK-GU                         PIC X(4) VALUE 'GU  '.
       01  KK-ISRT                       PIC X(4) VALUE 'ISRT'.
      ******************************************************************
      *    IMS RETURNCODES
      ******************************************************************
       01  KK-QC                         PIC X(4) VALUE 'QC  '.
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
      ******************************************************************
      *    MQ AREAS
      ******************************************************************
       01  WMQ-QMGR                      PIC X(48) VALUE SPACES.
       01  WMQ-HCONN                     PIC S9(09) BINARY.
       01  WMQ-OPTIONS                   PIC S9(09) BINARY.
       01  WMQ-COMPCODE                  PIC S9(09) BINARY.
       01  WMQ-REASON                    PIC S9(09) BINARY.
       01  WMQ-HOBJ                      PIC S9(09) BINARY.
       01  WMQ-HOBJ-RESPONS              PIC S9(09) BINARY.
       01  WMQ-RESPONSE                  PIC S9(09) BINARY.
       01  WMQ-BUFFLEN                   PIC S9(09) BINARY VALUE 1000.
       01  WMQ-DATALEN                   PIC S9(09) BINARY.
       01  WMQ-MSG-BUFF                      PIC X(40).
      ******************************************************************
      ***   MQ-DESKRIPTORER/KONSTANTER
      ******************************************************************
       01 MQM-CONSTANTS.
          COPY CMQV SUPPRESS.
      *
       01 MQM-GET-MESSAGE-OPTIONS.
          COPY CMQGMOV.
      *
       01 MQM-PUT-MESSAGE-OPTIONS.
          COPY CMQPMOV.
      *
       01 MQM-OBJECT-DESCRIPTOR.
          COPY CMQODV.
      *
       01 MQM-MESSAGE-DESCRIPTOR.
          COPY CMQMDV.
      ******************************************************************
      *    INDEX
      ******************************************************************
       01 IDX.
           05 LOOP-COUNT                  PIC 9(05) VALUE 0.
      ******************************************************************
      *    SWITCHES
      ******************************************************************
       01  SWITCHES.
           05 SW-INDICATOR                PIC X     VALUE '0'.
             88 ALL-OK                              VALUE '0'.
             88 NO-MORE-MESSAGES                    VALUE 'S'.
             88 INTERRUPTION                        VALUE 'F'.
             88 MQ-GET-MSG                          VALUE 'G'.
             88 MQ-PUT-MSG                          VALUE 'P'.
      
      *
       01  FILLER                        PIC X(32)  VALUE
                                       'X MODULE IMSMQ01 END WSS X'.
      *
       LINKAGE SECTION.
      *----------------
       01  IOPCB.
           05 IOLTERM                    PIC X(8).
           05 FILLER                     PIC X(2).
           05 STC-CODE                   PIC X(2).
           05 CDATE                      PIC X(4).
           05 CTIME                      PIC X(4).
           05 SEQNUM                     PIC X(4).
           05 MOD-NAME                   PIC X(8).
           05 USERID                     PIC X(8).
      *
       PROCEDURE DIVISION.
      **************************************************************
      *                                                            *
      *    A   MAINROUTINE                                         *
      *                                                            *
      **************************************************************
       A-MAINROUTINE SECTION.
      *
           PERFORM B-INITIATION
           PERFORM C-TRANSACTION-PROCESSING UNTIL NO-MORE-MESSAGES
                                                  OR
                                                  INTERRUPTION
           PERFORM Z-EXIT
           .
      ******************************************************************
      *
      *    B   INITIALIZATION
      *
      ******************************************************************
      
       B-INITIATION SECTION.
           MOVE 'IOPCB'                    TO AIB-PCB-NAME
           MOVE LENGTH OF INTRANS          TO AIB-IOAREA-LENGTH
      
           CONTINUE.
      ******************************************************************
      *
      *    C   TRANSACTION PROCESSING
      *
      ******************************************************************
      
       C-TRANSACTION-PROCESSING SECTION.
      *
           MOVE SPACES TO INDATA
                          OUTDATA
                          WMQ-MSG-BUFF
                          WS-DATE
                          WS-TIME
      
           CALL 'AIBTDLI' USING KK-GU
                               AIB
                               INTRANS
           END-CALL
      *
           PERFORM X-AIB-CHECK
      *
           DISPLAY '>' INDATA '<'
      *
           IF ALL-OK OR MQ-GET-MSG OR MQ-PUT-MSG
             EVALUATE INDATA1
               WHEN  'READQ'
                 SET MQ-GET-MSG              TO TRUE
                 MOVE INDATA3                TO MQOD-OBJECTNAME
                 PERFORM DA-MQ-CONN
               WHEN  'WRITE'
                 SET MQ-PUT-MSG              TO TRUE
                 MOVE INDATA3                TO MQOD-OBJECTNAME
                 PERFORM DA-MQ-CONN
               WHEN OTHER
                 DISPLAY 'IMSMQ01 START RC OK   '
                 DISPLAY 'IMSMQ01 ' LL
                 DISPLAY 'IMSMQ01 ' INDATA
      
                 MOVE INDATA               TO OUTDATA
      
                 ACCEPT WS-DATE              FROM DATE
                 ACCEPT WS-TIME              FROM TIME
                 DISPLAY 'IMSMQ01 RESPONSELL ' RESPONSELL
                 DISPLAY 'IMSMQ01 RESPONSEZZ ' RESPONSEZZ
             END-EVALUATE
      *
             PERFORM DH-WRITE-RESPONSE
      *
           END-IF
           DISPLAY 'IMSMQ01 AVSLUTAR RC ' STC-CODE
      
           CONTINUE.
      *    *************************************************************
      *    ***  CONNECT TO QUEUE MANAGER
      *    *************************************************************
       DA-MQ-CONN SECTION.
      
           CALL 'MQCONN' USING WMQ-QMGR
                               WMQ-HCONN
                               WMQ-COMPCODE
                               WMQ-REASON
                         END-CALL
      
           IF WMQ-COMPCODE = MQCC-OK
              DISPLAY 'IMSMQ01  MQCONN OK'
           END-IF
      
           IF WMQ-COMPCODE NOT = MQCC-OK
              DISPLAY 'IMSMQ01  MQCONN NOK'
              DISPLAY 'COMPCODE:   ' WMQ-COMPCODE
              DISPLAY 'REASON:     ' WMQ-REASON
           END-IF
           PERFORM DB-MQ-OPEN
           CONTINUE.
      
      *       **********************************************************
      *       ***  OPEN EXISTING MQ QUEUE
      *       **********************************************************
       DB-MQ-OPEN SECTION.
           MOVE MQOT-Q                   TO MQOD-OBJECTTYPE
           ADD  MQOO-INPUT-AS-Q-DEF MQOO-FAIL-IF-QUIESCING
                MQOO-OUTPUT  GIVING WMQ-OPTIONS
      
           CALL 'MQOPEN' USING WMQ-HCONN
                               MQOD
                               WMQ-OPTIONS
                               WMQ-HOBJ
                               WMQ-COMPCODE
                               WMQ-REASON
                         END-CALL
      
           IF WMQ-COMPCODE = MQCC-OK
              DISPLAY 'IMSMQ01  MQOPEN OK'
              EVALUATE TRUE
                WHEN MQ-GET-MSG
                    PERFORM DD-MQ-GET
                WHEN MQ-PUT-MSG
                    PERFORM DE-MQ-PUT
                WHEN OTHER
                    PERFORM DF-MQ-CLOSE
                    PERFORM DG-MQ-DISC
              END-EVALUATE
           END-IF
      
           IF WMQ-COMPCODE NOT = MQCC-OK
              DISPLAY 'IMSMQ01  MQOPEN NOK'
              DISPLAY 'COMPCODE:   ' WMQ-COMPCODE
              DISPLAY 'REASON:     ' WMQ-REASON
              PERFORM DG-MQ-DISC
           END-IF
           CONTINUE.
      *       **********************************************************
      *       ***  GET MQ MESSAGE
      *       **********************************************************
       DD-MQ-GET SECTION.
           MOVE MQMI-NONE                TO MQMD-MSGID
           MOVE MQCI-NONE                TO MQMD-CORRELID
           COMPUTE MQGMO-OPTIONS      = MQGMO-ACCEPT-TRUNCATED-MSG +
                                        MQGMO-CONVERT     +
                                        MQGMO-NO-WAIT
      
           PERFORM VARYING LOOP-COUNT FROM 1 BY 1 UNTIL
                                      LOOP-COUNT >= INDATA2 + 1
                                      OR
                                      WMQ-REASON = MQRC-NO-MSG-AVAILABLE
              CALL 'MQGET'  USING WMQ-HCONN
                                  WMQ-HOBJ
                                  MQMD
                                  MQGMO
                                  WMQ-BUFFLEN
                                  WMQ-MSG-BUFF
                                  WMQ-DATALEN
                                  WMQ-COMPCODE
                                  WMQ-REASON
              END-CALL
              IF WMQ-COMPCODE = MQCC-OK
                DISPLAY 'IMSMQ01  MQGET OK' ' <' WMQ-MSG-BUFF '>'
                MOVE WMQ-MSG-BUFF               TO OUTDATA
              END-IF
      
              IF WMQ-COMPCODE NOT = MQCC-OK
                 DISPLAY 'IMSMQ01  MQGET  NOK'
                 DISPLAY 'COMPCODE:   ' WMQ-COMPCODE
                 DISPLAY 'REASON:     ' WMQ-REASON
                 IF WMQ-REASON = MQRC-NO-MSG-AVAILABLE
                    MOVE ' > MQ QUEUE EMPTY'  TO OUTDATA
                 END-IF
              END-IF
           END-PERFORM
      
           PERFORM DF-MQ-CLOSE
           PERFORM DG-MQ-DISC
           CONTINUE.
      *       **********************************************************
      *       ***  PUT MQ MESSAGE
      *       **********************************************************
       DE-MQ-PUT SECTION.
           COMPUTE MQPMO-OPTIONS         = MQPMO-NO-SYNCPOINT +
                                           MQPMO-DEFAULT-CONTEXT
      
           MOVE 'IMSMQ01  MQPUT  OK'     TO WMQ-MSG-BUFF
           ADD  40                       TO WMQ-DATALEN
      
           MOVE MQMI-NONE                TO MQMD-MSGID
           MOVE MQCI-NONE                TO MQMD-CORRELID
           MOVE MQRO-NONE                TO MQMD-REPORT
      
           PERFORM VARYING LOOP-COUNT FROM 1 BY 1 UNTIL
                                           LOOP-COUNT >= INDATA2 + 1
              CALL 'MQPUT'     USING WMQ-HCONN
                                     WMQ-HOBJ
                                     MQMD
                                     MQPMO
                                     WMQ-DATALEN
                                     WMQ-MSG-BUFF
                                     WMQ-COMPCODE
                                     WMQ-REASON
              END-CALL
              IF WMQ-COMPCODE = MQCC-OK
                DISPLAY 'IMSMQ01  MQPUT  OK' ' <' WMQ-MSG-BUFF '>'
                MOVE 'MQ PUT MQ PERFORMED' TO OUTDATA
              END-IF
      
              IF WMQ-COMPCODE NOT = MQCC-OK
                 DISPLAY 'IMSMQ01  MQPUT  NOK'
                 DISPLAY 'COMPCODE:   ' WMQ-COMPCODE
                 DISPLAY 'REASON:     ' WMQ-REASON
              END-IF
           END-PERFORM
           PERFORM DF-MQ-CLOSE
           PERFORM DG-MQ-DISC
      
           CONTINUE.
      *       **********************************************************
      *       ***  CLOSE MQ QUEUE
      *       **********************************************************
       DF-MQ-CLOSE SECTION.
           MOVE MQCO-NONE                TO WMQ-OPTIONS
      
           CALL 'MQCLOSE' USING WMQ-HCONN
                                WMQ-HOBJ
                                WMQ-OPTIONS
                                WMQ-COMPCODE
                                WMQ-REASON
                          END-CALL
           IF WMQ-COMPCODE = MQCC-OK
              DISPLAY 'IMSMQ01  MQCLOSE OK'
           END-IF
      
      
           IF WMQ-COMPCODE NOT = MQCC-OK
              DISPLAY 'IMSMQ01  MQCLOSE NOK'
              DISPLAY 'COMPCODE:   ' WMQ-COMPCODE
              DISPLAY 'REASON:     ' WMQ-REASON
              PERFORM DG-MQ-DISC
           END-IF
           CONTINUE.
      *    *************************************************************
      *    ***  DISCONNECT FROM QUEUE MANAGER
      *    *************************************************************
       DG-MQ-DISC SECTION.
           CALL 'MQDISC' USING WMQ-HCONN
                               WMQ-COMPCODE
                               WMQ-REASON
                         END-CALL
      
           IF WMQ-COMPCODE = MQCC-OK
              DISPLAY 'IMSMQ01  MQDISC  OK'
           END-IF
      
           IF WMQ-COMPCODE NOT = MQCC-OK
              DISPLAY 'IMSMQ01  MQDISC  NOK'
              DISPLAY 'COMPCODE:   ' WMQ-COMPCODE
              DISPLAY 'REASON:     ' WMQ-REASON
           END-IF
      
           CONTINUE.
      ******************************************************************
      *
      *    D   SEND RESPONSE TO USER
      *
      ******************************************************************
       DH-WRITE-RESPONSE SECTION.
           COMPUTE RESPONSELL = LENGTH OF OUTDATA + 4
           MOVE LENGTH OF RESPONSE          TO AIB-IOAREA-LENGTH
      
           CALL 'AIBTDLI' USING KK-ISRT
                                AIB
                                RESPONSE
           END-CALL
      
           PERFORM X-AIB-CHECK
           SET ADDRESS OF IOPCB TO AIB-PCB-PTR
           DISPLAY 'IMSMQ01 WRITE RESPONSE RC ' STC-CODE
      
           CONTINUE.
      ******************************************************************
      *
      *    X   AIB RETURNCODE CHECK
      *
      ******************************************************************
       X-AIB-CHECK.
               EVALUATE TRUE
                   WHEN AIB-RETURN-CODE = AIB-HEX-0000 AND
                        AIB-REASON-CODE = AIB-HEX-0000
                     CONTINUE
                   WHEN AIB-RETURN-CODE = AIB-HEX-0900
                     SET ADDRESS OF IOPCB TO AIB-PCB-PTR
                     IF STC-CODE = KK-QC
                        DISPLAY 'IMSMQ01 GU RETURNCODE = ' STC-CODE ' '
                        SET NO-MORE-MESSAGES TO TRUE
                     ELSE
                        SET INTERRUPTION TO TRUE
                     END-IF
                   WHEN OTHER
                     SET ADDRESS OF IOPCB TO AIB-PCB-PTR
                     DISPLAY 'IMSMQ01 UNEXPECTED RETURNCODE = ' STC-CODE
               END-EVALUATE
           CONTINUE.
      ******************************************************************
      *
      *    Z   EXIT
      *
      ******************************************************************
       Z-EXIT SECTION.
      
           GOBACK.

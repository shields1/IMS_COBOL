       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID. IMSMQ02.
      ******************************************************************
      *
      *    MODULE      IMSMQ02
      *                MPP
      *                IMS TRIGGER TRANSACTION
      *                1) TRIGGERED BY MQ TRIGGER MONITOR
      *                2) GU AGAINST IOPCB TO GET WHICH MQ QUEUE
      *                    HAS THER REAL MESSAGE
      *                3) GET THE REAL MESSAGE FROM MQ QUEUE
      *
      *
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
      *     DA   = SET MQ OPTIONS
      *     DB   = CONNECT TO QUEUE MANAGER
      *     DC   = OPEN EXISTING MQ QUEUE
      *     DD   = GET MQ MESSAGE
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
      *    AIBTDLI - IMS APPLICATION INTERFACE
      *    MQCONN  - CONNECT TO QUEUE MANAGER
      *    MQOPEN  - OPEN EXISTING MQ QUEUE
      *    MQGET   - GET MQ MESSAGE
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
                                       'X MODULE IMSMQ02 START WSS X'.
      ******************************************************************
      *    MQ WORKAREAS
      ******************************************************************
       01 A-MQTMC-USERDATA                PIC X(128)     VALUE SPACE.
       01 A-MQTMC-OPER.
           05 A-MQTMC-BACKOUT-LIMIT-X.
              10 A-MQTMC-BACKOUT-LIMIT    PIC 9(03)      VALUE 3.
           05 A-MQTMC-ERROR-QUEUE-SUFFIX  PIC X(12)      VALUE '.ERROR'.
           05 A-MQTMC-ERROR-QUEUE-ONAME   PIC X(48)      VALUE SPACE.
           05 A-MQTMC-CONVERT             PIC X(01)      VALUE 'N'.
      ******************************************************************
      *    APPLICATION INTERFACE BLOCK (AIB)
      ******************************************************************
       01 AIB.
         05 AIB-ID                        PIC X(08)      VALUE 'DFSAIB'.
         05 AIB-LEN                       PIC 9(09) COMP VALUE 128.
         05 AIB-SUB-FUNC.
           10 AIB-SUB-FUNC-1              PIC X(04)      VALUE SPACE.
           10 AIB-SUB-FUNC-2              PIC X(04)      VALUE SPACE.
         05 AIB-PCB-NAME                  PIC X(08)      VALUE SPACE.
         05 FILLER                        PIC X(16)      VALUE SPACE.
         05 AIB-IOAREA-LENGTH             PIC 9(09) COMP VALUE ZERO.
         05 AIB-IOAREA-USED               PIC 9(09) COMP.
         05 FILLER                        PIC X(12)      VALUE SPACE.
         05 AIB-RETURN-CODE               PIC 9(09) COMP.
         05 AIB-REASON-CODE               PIC 9(09) COMP.
         05 FILLER                        PIC X(04)      VALUE SPACE.
         05 AIB-PCB-PTR                             POINTER.
         05 FILLER                        PIC X(48)      VALUE SPACE.
      ******************************************************************
      *    AIB RETURNCODES
      ******************************************************************
       01 AIB-RETURKODER.
          05 AIB-HEX-0000                 PIC 9(09) COMP  VALUE 0.
          05 AIB-HEX-000C                 PIC 9(09) COMP  VALUE 12.
          05 AIB-HEX-0100                 PIC 9(09) COMP  VALUE 256.
          05 AIB-HEX-0104                 PIC 9(09) COMP  VALUE 260.
          05 AIB-HEX-0208                 PIC 9(09) COMP  VALUE 520.
          05 AIB-HEX-0210                 PIC 9(09) COMP  VALUE 528.
          05 AIB-HEX-0214                 PIC 9(09) COMP  VALUE 532.
          05 AIB-HEX-0218                 PIC 9(09) COMP  VALUE 536.
          05 AIB-HEX-0900                 PIC 9(09) COMP  VALUE 2304.
      ******************************************************************
      *    IMS IOAREA
      ******************************************************************
       01  IOPCB-DATA.
           10 LL                          PIC S9(3) COMP.
           10 ZZ                          PIC S9(3) COMP.
           COPY CMQTMC2V.
           10 FILLER                      PIC X(1000).
       01  CC-CONSTANTS.
      ******************************************************************
      *    IMS DLICALLS
      ******************************************************************
           05  CC-GU                      PIC X(04)       VALUE 'GU  '.
           05  CC-ISRT                    PIC X(04)       VALUE 'ISRT'.
      ******************************************************************
      *    IMS RETURKODER
      ******************************************************************
           05  CC-QC                      PIC X(02)       VALUE 'QC'.
      ******************************************************************
      *    MQ IOAREA
      ******************************************************************
       01  WMQ-QMGR                       PIC X(48)       VALUE SPACES.
       01  WMQ-HCONN                      PIC S9(09) BINARY.
       01  WMQ-OPTIONS                    PIC S9(09) BINARY.
       01  WMQ-COMPCODE                   PIC S9(09) BINARY.
       01  WMQ-REASON                     PIC S9(09) BINARY.
       01  WMQ-HOBJ                       PIC S9(09) BINARY.
       01  WMQ-HOBJ-RESPONS               PIC S9(09) BINARY.
       01  WMQ-RESPONSE                   PIC S9(09) BINARY.
       01  WMQ-BUFFLEN                    PIC S9(09) BINARY VALUE 1000.
       01  WMQ-DATALEN                    PIC S9(09) BINARY.
       01  WMQ-MSG-BUFF                   PIC X(40).
      ******************************************************************
      ***   MQ-DESCRIPTORS/CONSTANTER
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
           05 IDX-LOOP-COUNT              PIC 9(05)   VALUE 0.
      ******************************************************************
      *    SWITCHES
      ******************************************************************
       01  SWITCHES.
           05 SW-INDICATOR                PIC X       VALUE '0'.
             88 ALL-OK                                VALUE '0'.
             88 NO-MORE-MESSAGES                      VALUE 'S'.
             88 INTERRUPTION                          VALUE 'F'.
      *
       01  FILLER                        PIC X(32)    VALUE
                                       'X MODULE IMSMQ02 END WSS X'.
      *
       LINKAGE SECTION.
      *----------------
       01  IOPCB.
           05 IOLTERM                     PIC X(08).
           05 FILLER                      PIC X(02).
           05 STC-CODE                    PIC X(02).
           05 CDATE                       PIC X(04).
           05 CTIME                       PIC X(04).
           05 SEQNUM                      PIC X(04).
           05 MOD-NAME                    PIC X(08).
           05 USERID                      PIC X(08).
      *
       PROCEDURE DIVISION.
      **************************************************************
      *                                                            *
      *    A   MAINROUTINE                                          *
      *                                                            *
      **************************************************************
       A-MAINROUTINE SECTION.
      
           PERFORM B-INITIATION
           PERFORM C-TRANSACTION-PROCESSING UNTIL NO-MORE-MESSAGES OR
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
           MOVE LENGTH OF IOPCB-DATA       TO AIB-IOAREA-LENGTH
      
           CONTINUE.
      ******************************************************************
      *
      *    C   TRANSACTION PROCESSING
      *
      ******************************************************************
       C-TRANSACTION-PROCESSING SECTION.
      
           MOVE SPACES TO IOPCB-DATA
                          WMQ-MSG-BUFF
           CALL 'AIBTDLI' USING CC-GU
                                AIB
                                IOPCB-DATA
           END-CALL
      
           PERFORM X-AIB-CHECK
           DISPLAY 'IMSMQ02 IMSGU OK' ' <' MQTMC '>'
      
           PERFORM DA-MQTMC-PARMS
           
           MOVE MQTMC-QNAME             TO MQOD-OBJECTNAME
           PERFORM DB-MQ-CONN
      
      *
           DISPLAY 'IMSMQ02 AVSLUTAR RC ' STC-CODE
      *
           CONTINUE.
      ******************************************************************
      *
      *    D   MQ PROCESSING
      *
      ******************************************************************
       DA-MQTMC-PARMS SECTION.
           MOVE MQTMC-USERDATA          TO A-MQTMC-USERDATA
      
           UNSTRING A-MQTMC-USERDATA DELIMITED BY ';'
           INTO A-MQTMC-BACKOUT-LIMIT-X
                A-MQTMC-ERROR-QUEUE-SUFFIX
                A-MQTMC-ERROR-QUEUE-ONAME
                A-MQTMC-CONVERT
           ON OVERFLOW
                DISPLAY 'IMSMQ02  OVERFLOW OF MQ TMC USERDATA'
                PERFORM Z-EXIT
           END-UNSTRING
      
           IF A-MQTMC-BACKOUT-LIMIT-X NOT NUMERIC
                MOVE 3                  TO A-MQTMC-BACKOUT-LIMIT
           END-IF
           IF A-MQTMC-ERROR-QUEUE-SUFFIX <= SPACE
                MOVE '.ERROR'           TO A-MQTMC-ERROR-QUEUE-SUFFIX
           END-IF
           IF A-MQTMC-CONVERT NOT = 'Y'
                MOVE 'N'                TO A-MQTMC-CONVERT
           END-IF
      
           CONTINUE.
      *    *************************************************************
      *    ***  CONNECT TO QUEUE MANAGER
      *    *************************************************************
       DB-MQ-CONN SECTION.
      
           CALL 'MQCONN' USING WMQ-QMGR
                               WMQ-HCONN
                               WMQ-COMPCODE
                               WMQ-REASON
           END-CALL
      
           IF WMQ-COMPCODE = MQCC-OK
              DISPLAY 'IMSMQ02  MQCONN OK'
              PERFORM DC-MQ-OPEN
           END-IF
      
           IF WMQ-COMPCODE NOT = MQCC-OK
              DISPLAY 'IMSMQ02  MQCONN NOK'
              DISPLAY 'COMPCODE:   ' WMQ-COMPCODE
              DISPLAY 'REASON:     ' WMQ-REASON
           END-IF
      
           CONTINUE.
      *       **********************************************************
      *       ***  OPEN EXISTING MQ QUEUE
      *       **********************************************************
       DC-MQ-OPEN SECTION.
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
              DISPLAY 'IMSMQ02  MQOPEN OK'
              PERFORM DD-MQ-GET
           END-IF
      
           IF WMQ-COMPCODE NOT = MQCC-OK
              DISPLAY 'IMSMQ02  MQOPEN NOK'
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
             DISPLAY 'IMSMQ02  MQGET OK' ' <' WMQ-MSG-BUFF '>'
             PERFORM DF-MQ-CLOSE
           END-IF
      
           IF WMQ-COMPCODE NOT = MQCC-OK
              DISPLAY 'IMSMQ02  MQGET  NOK'
              DISPLAY 'COMPCODE:   ' WMQ-COMPCODE
              DISPLAY 'REASON:     ' WMQ-REASON
              IF WMQ-REASON = MQRC-NO-MSG-AVAILABLE
                 DISPLAY 'IMSMQ02  MQ QUEUE EMPTY'
              END-IF
              PERFORM DF-MQ-CLOSE
              PERFORM DG-MQ-DISC
           END-IF
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
              DISPLAY 'IMSMQ02  MQCLOSE OK'
              PERFORM DG-MQ-DISC
           END-IF
      
           IF WMQ-COMPCODE NOT = MQCC-OK
              DISPLAY 'IMSMQ02  MQCLOSE NOK'
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
              DISPLAY 'IMSMQ02  MQDISC  OK'
           END-IF
      
           IF WMQ-COMPCODE NOT = MQCC-OK
              DISPLAY 'IMSMQ02  MQDISC  NOK'
              DISPLAY 'COMPCODE:   ' WMQ-COMPCODE
              DISPLAY 'REASON:     ' WMQ-REASON
           END-IF
      
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
                     IF STC-CODE = CC-QC
                        DISPLAY 'IMSMQ02 GU RETURKOD = ' STC-CODE ' '
                        SET NO-MORE-MESSAGES TO TRUE
                     ELSE
                        SET INTERRUPTION TO TRUE
      
                     END-IF
                   WHEN OTHER
                     SET ADDRESS OF IOPCB TO AIB-PCB-PTR
                     DISPLAY 'IMSMQ02 OVÄNTAD RETURKOD = ' STC-CODE
               END-EVALUATE
           CONTINUE.
      ******************************************************************
      *
      *    Z   EXIT
      *
      ******************************************************************
       Z-EXIT SECTION.
      
           GOBACK.

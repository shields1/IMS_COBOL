       IDENTIFICATION DIVISION.
       PROGRAM-ID.     IMSINQY.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER                   PIC X(32)   VALUE
                                    'XXX MODUL IMSINQY START WSS XXX'.
       01  KONSTANTER.
           05 KK-INQY               PIC X(4)         VALUE 'INQY'.
           05 KK-ENVIRON            PIC X(8)         VALUE 'ENVIRON'.
           05 KK-IOPCB              PIC X(8)         VALUE 'IOPCB'.
       01  SWITCHES.
           05 SW-INDICATOR          PIC X     VALUE '0'.
             88 ALL-OK                        VALUE '0'.
             88 ABEND                         VALUE 'F'.
             88 PCB-ERROR                     VALUE 'P'.
             88 AIB-ERROR                     VALUE 'A'.
       01  TB-HEX-KODER.
         05  TB-TABLE-RETURNCODES.
           10  HEX-0000             PIC X(04)       VALUE X'00000000'.
           10                       PIC X(04)       VALUE '0000'.
           10  HEX-000C             PIC X(04)       VALUE X'0000000C'.
           10                       PIC X(04)       VALUE '000C'.
           10  HEX-0100             PIC X(04)       VALUE X'00000100'.
           10                       PIC X(04)       VALUE '0100'.
           10  HEX-0104             PIC X(04)       VALUE X'00000104'.
           10                       PIC X(04)       VALUE '0104'.
           10  HEX-0208             PIC X(04)       VALUE X'00000208'.
           10                       PIC X(04)       VALUE '0208'.
           10  HEX-0210             PIC X(04)       VALUE X'00000210'.
           10                       PIC X(04)       VALUE '0210'.
           10  HEX-0218             PIC X(04)       VALUE X'00000218'.
           10                       PIC X(04)       VALUE '0218'.
           10  HEX-0610             PIC X(04)       VALUE X'00000610'.
           10                       PIC X(04)       VALUE '0610'.
           10  HEX-0900             PIC X(04)       VALUE X'00000900'.
           10                       PIC X(04)       VALUE '0900'.
           10  LAST-KEY             PIC X(04)       VALUE HIGH-VALUE.
           10                       PIC X(04)       VALUE '????'.
      
           05 REDEFINES TB-TABLE-RETURNCODES.
      
             10                                 OCCURS 10
                                                INDEXED BY
                                                        TABLE-IX
                                                        TABLE-IX-START.
               15 TB-RETKOD         PIC X(04).
               15 TB-RETKOD-CHAR    PIC X(04).
       01  WORKAREAS.
      *----------------------------------------------------------------
      *     NOTIFICATIONAREA
      *----------------------------------------------------------------
         05  NOTIFICATIONAREA.
           10  MODULEDESCRIPTION    PIC X(30).
           10  MODULEAREA.
             15  MODULENAME         PIC X(8).
             15  NOTIFICATION-TAB.
               20  NOTIFICATIONTEXT OCCURS 17 TIMES
                                    PIC X(72).
      *----------------------------------------------------------------
      *     A I B  -  A R E A
      *----------------------------------------------------------------
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
         05 AIB-RETURN-CODE         PIC X(04).
         05 AIB-REASON-CODE         PIC X(04).
         05 FILLER                  PIC X(04)          VALUE SPACE.
         05 AIB-PCB-PTR             POINTER.
         05 FILLER                  PIC X(48)          VALUE SPACE.
      *
       01  FILLER                   PIC X(32)   VALUE
                                    'XXX MODUL IMSINQY END WSS XXXX'.
      
      *
       LINKAGE SECTION.
       01  REQUEST.
           05 INQY-TYPE              PIC X(8).
           05 INQYENV-LENGTH         PIC S9(5) COMP.
      *
       01  ANSWER.
           05 INQYENV-RETURNCODE     PIC 9(0008) COMP.
           05 INQYENV-REASONCODE     PIC 9(0008) COMP.
           05 AA-INQY.
             10 INQYENV-IMSID          PIC X(8).
             10 INQYENV-RELEASE-LVL    PIC S9(8)    COMP.
             10 INQYENV-C-REG-TYPE     PIC X(8).
             10 INQYENV-A-REG-TYPE     PIC X(8).
             10 INQYENV-REG-ID         PIC S9(8)   COMP.
             10 INQYENV-APPL-PGM       PIC X(8).
             10 INQYENV-PSB-NAME       PIC X(8).
             10 INQYENV-TRANS-NAME     PIC X(8).
             10 INQYENV-USERID         PIC X(8).
             10 INQYENV-GROUP-NAME     PIC X(8).
             10  FILLER                PIC X(8).
             10 INQYENV-APARM-X.
               15 INQYENV-APARM-P      POINTER.
             10  FILLER                REDEFINES INQYENV-APARM-X.
               15 INQYENV-APARM        PIC S9(8)   COMP.
             10  FILLER                PIC X(100).
      *
       01  PCBMASK.
           05 PCB-DBD-NAME          PIC X(08).
           05 PCB-SEG-LEVEL         PIC X(02).
           05 PCB-STATUS-CODE       PIC X(02).
           05 PCB-PROC-OPT          PIC X(04).
           05                       PIC S9(5) COMP.
           05 PCB-SEG-NAME-FB       PIC X(08).
           05 PCB-LENGTH-FB-KEY     PIC S9(5) COMP.
           05 PCB-NUM-SENSEG        PIC S9(5) COMP.
           05 PCB-KEY-FB-AREA       PIC X(100).
      
       PROCEDURE DIVISION USING  REQUEST
                                 ANSWER.
      *-----------------------------------------------------------------
      *    A-MAINSECTION
      *-----------------------------------------------------------------
       A-MAINSECTION SECTION.
      
           PERFORM INITIATE-NOTIAREA
      
           PERFORM INQ-CALL
      
           PERFORM Z-EXIT
      
           GOBACK.
      *-----------------------------------------------------
      *    INITIATE MESSAGE AREA
      *-----------------------------------------------------
       INITIATE-NOTIAREA SECTION.
           MOVE 'PERFORM INQ CALL TO IMS'   TO MODULEDESCRIPTION
           MOVE 'IMSINQY'                   TO MODULENAME
           CONTINUE.
      *-----------------------------------------------------
      *    INQ CALL
      *-----------------------------------------------------
       INQ-CALL SECTION.
      *--------------------------------------------------------
           MOVE LENGTH OF AIB      TO AIB-LEN
           MOVE KK-ENVIRON         TO AIB-SUB-FUNC
           MOVE KK-IOPCB           TO AIB-PCB-NAME
           MOVE LENGTH OF AA-INQY  TO AIB-IOAREA-LENGTH
      *----
           CALL 'AIBTDLI' USING  KK-INQY
                                 AIB
                                 AA-INQY
      *----
           IF    AIB-RETURN-CODE = HEX-0000
           AND   AIB-REASON-CODE = HEX-0000
             DISPLAY 'IMSID           : ' INQYENV-IMSID
             DISPLAY 'RELEASE-LVL     : ' INQYENV-RELEASE-LVL
             DISPLAY 'C-REG-TYPE      : ' INQYENV-C-REG-TYPE
             DISPLAY 'A-REG-TYPE      : ' INQYENV-A-REG-TYPE
             DISPLAY 'REG-ID          : ' INQYENV-REG-ID
             DISPLAY 'APPL-PGM        : ' INQYENV-APPL-PGM
             DISPLAY 'PSB-NAME        : ' INQYENV-PSB-NAME
             DISPLAY 'TRANS-NAME      : ' INQYENV-TRANS-NAME
             DISPLAY 'USERID          : ' INQYENV-USERID
             DISPLAY 'GROUP-NAME      : ' INQYENV-GROUP-NAME
             DISPLAY 'INQYENV-APARM   : ' INQYENV-APARM
             CONTINUE
           ELSE
              IF AIB-RETURN-CODE = HEX-0900
                 SET ADDRESS OF PCBMASK TO AIB-PCB-PTR
                 SET PCB-ERROR          TO TRUE
              ELSE
                 SET AIB-ERROR          TO TRUE
              END-IF
      
              MOVE AIB-REASON-CODE      TO INQYENV-REASONCODE
              MOVE AIB-RETURN-CODE      TO INQYENV-RETURNCODE
              PERFORM WRITE-IMS-INFO
           END-IF
           CONTINUE.
      *----------------------------------------------------------------
      *    WRITE ERROR INFORMATION
      *-----------------------------------------------------------------
       WRITE-IMS-INFO SECTION.
           EVALUATE TRUE
           WHEN PCB-ERROR
              STRING
               'DBDNAME         : ' PCB-DBD-NAME
               DELIMITED BY SIZE INTO NOTIFICATIONTEXT(5)
              END-STRING
              STRING
               'IMS-RETURKOD    : ' PCB-STATUS-CODE
               DELIMITED BY SIZE INTO NOTIFICATIONTEXT(6)
              END-STRING
              STRING
               'PCB PROC-OPTION : ' PCB-PROC-OPT
               DELIMITED BY SIZE INTO NOTIFICATIONTEXT(7)
              END-STRING
              STRING
               'SEGMENT NAME    : ' PCB-SEG-NAME-FB
               DELIMITED BY SIZE INTO NOTIFICATIONTEXT(8)
              END-STRING
      *-----------------------------------------------------------------
      *   *** AIB ERROR ***
      *-----------------------------------------------------------------
           WHEN AIB-ERROR
              EVALUATE TRUE
      *-----------------------------------------------------------------
      *  FOR A SMALL OUTPUT AREA
      *-----------------------------------------------------------------
              WHEN AIB-RETURN-CODE = HEX-0100
              AND AIB-REASON-CODE = HEX-000C
                 STRING
                  'FOR A SMALL OUTPUT AREA                       '
                   DELIMITED BY SIZE INTO NOTIFICATIONTEXT(2)
                 END-STRING
      
      *-----------------------------------------------------------------
      *  INVALID PCB NAME / NOT GENERATED IN PSB
      *-----------------------------------------------------------------
              WHEN AIB-RETURN-CODE = HEX-0104
              AND AIB-REASON-CODE = HEX-0208
                 STRING
                  'INVALID PCB NAME / NOT GENERATED IN PSB       '
                   DELIMITED BY SIZE INTO NOTIFICATIONTEXT(2)
                 END-STRING
      *-----------------------------------------------------------------
      *  OUTPUT AREA NOT DEFINED, NO DATA IN RETURN
      *-----------------------------------------------------------------
              WHEN AIB-RETURN-CODE = HEX-0104
              AND AIB-REASON-CODE = HEX-0610
                 STRING
                  'OUTPUT AREA NOT DEFINED, NO DATA IN RETURN    '
                   DELIMITED BY SIZE INTO NOTIFICATIONTEXT(2)
                 END-STRING
      
      *-----------------------------------------------------------------
      *  OUTPUT AREA LENGTH = 0, NO DATA IN RETURN
      *-----------------------------------------------------------------
              WHEN AIB-RETURN-CODE = HEX-0104
              AND AIB-REASON-CODE = HEX-0210
                 STRING
                  'OUTPUT AREA LENGTH = 0, NO DATA IN RETURN  '
                   DELIMITED BY SIZE INTO NOTIFICATIONTEXT(2)
                 END-STRING
      
      *-----------------------------------------------------------------
      *  SUBFUNCTION UNKNOWN
      *-----------------------------------------------------------------
              WHEN AIB-RETURN-CODE = HEX-0104
              AND AIB-REASON-CODE = HEX-0218
                 STRING
                  'SUBFUNCTION UNKNOWN          '
                   DELIMITED BY SIZE INTO NOTIFICATIONTEXT(2)
                 END-STRING
              END-EVALUATE
      
      *-----------------------------------------------------------------
      *  EDIT A RETURN CODE AND REASON CODE
      *-----------------------------------------------------------------
              STRING
               '   AIB-PCB-NAME      : ' AIB-PCB-NAME '.'
                DELIMITED BY SIZE INTO NOTIFICATIONTEXT(3)
              END-STRING
      *-----------------------------------------------------------------
      *  AIB-RETURN CODE
      *-----------------------------------------------------------------
              SET TABLE-IX                TO +1
              PERFORM VARYING TABLE-IX FROM +1 BY +1
                 UNTIL TB-RETKOD(TABLE-IX) = HIGH-VALUE
                 OR TB-RETKOD(TABLE-IX) = AIB-REASON-CODE
              END-PERFORM
              STRING
               ' -  AIB-REASON-CODE   : '  TB-RETKOD-CHAR(TABLE-IX) '.'
                DELIMITED BY SIZE INTO NOTIFICATIONTEXT(4)
              END-STRING
      
      *-----------------------------------------------------------------
      *  AIB-REASON CODE
      *-----------------------------------------------------------------
      
              SET TABLE-IX                TO +1
              PERFORM VARYING TABLE-IX FROM +1 BY +1
                UNTIL TB-RETKOD(TABLE-IX) = HIGH-VALUE
                OR TB-RETKOD(TABLE-IX) = AIB-RETURN-CODE
              END-PERFORM
              STRING
               ' -  AIB-RETURN-CODE   : '  TB-RETKOD-CHAR(TABLE-IX) '.'
                 DELIMITED BY SIZE INTO NOTIFICATIONTEXT(5)
              END-STRING
           END-EVALUATE
      *
           CONTINUE.
      ******************************************************************
      *
      *    Z   EXIT
      *
      ******************************************************************
       Z-EXIT SECTION.
      *
           IF ALL-OK
              CONTINUE
           ELSE
              DISPLAY MODULEDESCRIPTION
              DISPLAY MODULENAME
              DISPLAY NOTIFICATIONTEXT (01)
              DISPLAY NOTIFICATIONTEXT (02)
              DISPLAY NOTIFICATIONTEXT (03)
              DISPLAY NOTIFICATIONTEXT (04)
              DISPLAY NOTIFICATIONTEXT (05)
              DISPLAY NOTIFICATIONTEXT (06)
              DISPLAY NOTIFICATIONTEXT (07)
              DISPLAY NOTIFICATIONTEXT (08)
              DISPLAY NOTIFICATIONTEXT (09)
              DISPLAY NOTIFICATIONTEXT (10)
              DISPLAY NOTIFICATIONTEXT (11)
              DISPLAY NOTIFICATIONTEXT (12)
              DISPLAY NOTIFICATIONTEXT (13)
              DISPLAY NOTIFICATIONTEXT (14)
              DISPLAY NOTIFICATIONTEXT (15)
              DISPLAY NOTIFICATIONTEXT (16)
              DISPLAY NOTIFICATIONTEXT (17)
           END-IF
      *
           CONTINUE.
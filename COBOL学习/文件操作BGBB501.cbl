       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             BGBB501.
      * *************************************************************
      * **      �v���O��������                                    ***
      * **            ���m����X�o�׎��уf�[�^�ҏW�@              ***
      * **      �s�o���j�^�[                                      ***
      * **                                                        ***
      * **      �c�a�l�r                                          ***
      * **                                                        ***
      * **      �O���C���^�[�t�F�[�X                              ***
      * **                                                        ***
      * **      �v���O����                                        ***
      * **            �l�`�h�m                                    ***
      * *************************************************************
      ******************************************************************
      *           IS
      *           �@�@�@�@�m�W���u�h�c�F�a�f�a�a�T�n
      *           IL2
      *           �a�P�P�P�F�ҏW�^�S���f�[�^�ҏW
      *           �i�u�����@�O�P�D�O�O�j
      *           IL5
      *           �@�@�@�@�@�������@�ύX���e�����Ǘ��@������
      *           IL
      *           �@�@�@�@�@�@���@�@�@�@�@�H�����@�@�@�@�@�@�@
      *           IL
      *           �@�@�@�@�@�@�O�@�F�@�a�f�a�m�|�O�O�Q�X�@�@�@
      *           IE
      ******************************************************************
       ENVIRONMENT             DIVISION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT ZFL572A-F  ASSIGN TO ZFL572A.
           SELECT YF55204A-F ASSIGN TO YF55204A.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
       FD  ZFL572A-F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD
           RECORDING MODE IS F                                          AIXMODE
           DATA RECORD IS ZFL572A-REC.
       01  ZFL572A-REC.
           COPY ZFL572A.
       FD  YF55204A-F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD
           RECORDING MODE IS F                                          AIXMODE
           DATA RECORD IS YF55204A-REC.
       01  YF55204A-REC.
           COPY YF55204A.
      *
       WORKING-STORAGE         SECTION.
      ******************************************************************
      *    ���[�N�ϐ��G���A                                            *
      ******************************************************************
       01  WK-CNV-AREA.
         03  WK-CNV-AREA-START          PIC X(14)
             VALUE 'CNV-WORK-START'.
             COPY CBLCNS02.
         03  WK-CNV-AREA-END            PIC X(12)
             VALUE 'CNV-WORK-END'.
      ******************************************************************
      *                ���[�N  �G���A                                  *
      ******************************************************************
      *
       01  WORK-AREA.
         03  WK-START                   PIC X(10) VALUE 'WORK-START'.
         03  SW-EOF                     PIC X(01).
         03  WK-CTR-INP                 PIC 9(07).
         03  WK-CTR-OUT                 PIC 9(07).
      *
         03  WK-NO-HKK-2                PIC X(02).
      *
         03  WK-END                     PIC X(08) VALUE 'WORK-END'.
      *
       01  YAB110.
           COPY YAB110.
       01  ZFL572A.
           COPY ZFL572A.
       01  YAG018.
           COPY YAG018.
       01  YAG044.
           COPY YAG044.
       01  YAB111.
           COPY YAB111.
       01  YAB153.
           COPY YAB153.
       01  YAB165.
           COPY YAB165.
       01  YF55204A.
           COPY YF55204A.
       01  YAG005.
           COPY YAG005.
       LINKAGE                 SECTION.
      *
       PROCEDURE               DIVISION.
      ******************************************************************
      *        MAIN                                                    *
      ******************************************************************
       MAIN                    SECTION.
      *-------- ��������
           PERFORM FST-RTN THRU FST-RTN-EXIT
      *-------- ���͏���
           PERFORM RED-RTN THRU RED-RTN-EXIT
           PERFORM
             UNTIL SW-EOF NOT = SPACE
      *-------- �ҏW����
               PERFORM EDT-RTN THRU EDT-RTN-EXIT
      *-------- �o�͏���
               PERFORM WRT-RTN THRU WRT-RTN-EXIT
      *-------- ���͏���
               PERFORM RED-RTN THRU RED-RTN-EXIT
           END-PERFORM
      *-------- �I������
           PERFORM END-RTN THRU END-RTN-EXIT
      *
           .
       MAIN-EXIT.
           GOBACK.
      ******************************************************************
      *         FST-RTN : ���������@�@�@�@�@�@�@�@�@�@�@               *
      ******************************************************************
       FST-RTN                 SECTION.
      *
      *-------- �t�@�C���E�I�[�v������
           PERFORM OPN-FIL THRU OPN-FIL-EXIT
      *-------- �N���A����
           PERFORM INT-RTN THRU INT-RTN-EXIT
      *
      *  �m�a�g�a�s�P�P�O�n�i�n�a�������t�E�����擾
           MOVE SPACE
             TO YAB110
           MOVE 'BGBB501'
             TO QA-NO-PGM           IN YAB110
           CALL 'ZAGB110' USING YAB110
      *
           .
       FST-RTN-EXIT.
           EXIT.
      ******************************************************************
      *         OPN-FIL : �t�@�C���E�I�[�v�������@�@�@�@               *
      ******************************************************************
       OPN-FIL                 SECTION.
      *
           OPEN INPUT ZFL572A-F
           OPEN OUTPUT YF55204A-F
      *
           .
       OPN-FIL-EXIT.
           EXIT.
      ******************************************************************
      *         INT-RTN : �N���A�����@�@�@�@�@�@�@�@�@�@               *
      ******************************************************************
       INT-RTN                 SECTION.
      *
           MOVE SPACE
             TO SW-EOF
           MOVE ZERO
             TO WK-CTR-INP
           MOVE ZERO
             TO WK-CTR-OUT
      *
           .
       INT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *         RED-RTN : ���͏����@�@�@�@�@�@�@�@�@�@�@               *
      ******************************************************************
       RED-RTN                 SECTION.
      *
           READ ZFL572A-F INTO ZFL572A
             AT END
               MOVE '1'
                 TO SW-EOF
           END-READ
           IF  SW-EOF = SPACE
               COMPUTE WK-CTR-INP = WK-CTR-INP + 1
           END-IF
      *
           .
       RED-RTN-EXIT.
           EXIT.
      ******************************************************************
      *         EDT-RTN : �ҏW�����@�@�@�@�@�@�@�@�@�@�@               *
      ******************************************************************
       EDT-RTN                 SECTION.
      *
           MOVE SPACE
             TO YF55204A
           MOVE ZERO
             TO NO-SEQ-LEN13        IN GP-X552TB04 IN YF55204E-ENT
             IN YF55204A
           MOVE ZERO
             TO KS-CST-NNY-SJ       IN GP-X552TB04 IN YF55204E-ENT
             IN YF55204A
           MOVE ZERO
             TO KS-SHK-JSK          IN GP-X552TB04 IN YF55204E-ENT
             IN YF55204A
           MOVE ZERO
             TO KS-BOX-SHUYO        IN GP-X552TB04 IN YF55204E-ENT
             IN YF55204A
           MOVE ZERO
             TO NO-REF              IN GP-X552TB04 IN YF55204E-ENT
             IN YF55204A
           MOVE ZERO
             TO BKA-NOW-13          IN GP-X552TB04 IN YF55204E-ENT
             IN YF55204A
      *
      *  �o�׎��ѓ��t
           MOVE DT-PAYOUT           IN ZFL572A
             TO DT-SHK-JSK          IN YF55204A
      *
      *  �o�׎��ѐ�
           IF  QA-SIGN IN ZFL572A ='-'
               COMPUTE KS-SHK-JSK IN YF55204A
                 = QA-KS-N8 IN ZFL572A * -1
             ELSE
               MOVE QA-KS-N8            IN ZFL572A
                 TO KS-SHK-JSK          IN YF55204A
           END-IF
      *
      *  ���s�ԍ�
           MOVE QA-GKA-DPT-4        IN ZFL572A
             TO BG-HKK              IN YF55204A
      *
      *  �o�ו���R�[�h
      *-------- �o�ו���R�[�h�擾
           PERFORM DPT-GET THRU DPT-GET-EXIT
      *
      *  �s�q�i�ԁE�V�X�e��
      *  �m�a�g�a�s�O�Q�P�n�s�q�i�ԃV�X�e���ϊ��T�u
           MOVE SPACE
             TO YAG018
           MOVE HB-TR-Z             IN ZFL572A
             TO QA-HB-TR            IN YAG018
           CALL 'ZAEB601' USING YAG018
           MOVE QA-HB-TR            IN YAG018
             TO HB-TR-Z-SYS         IN YF55204A
      *
      *  �s�q�i�ԁE�ݕϖ���
      *  �m�a�g�a�s�O�T�P�n�s�q�i�ԁE�ݕϖ����ϊ��T�u
           MOVE SPACE
             TO YAG044
           MOVE '01'
             TO QA-CD-PGMFUNC       IN YAG044
           MOVE HB-TR-Z             IN ZFL572A
             TO QA-HB-TR            IN YAG044
           CALL 'ZAEA151' USING YAG044
           MOVE HB-TR-SPP-MU        IN YAG044
             TO HB-TR-SPP-MU        IN YF55204A
      *
      *  �񋟃f�[�^�쐬�i�n�a
           MOVE 'BGBB5'
             TO CD-BT-TKY           IN YF55204A
      *
      *  �o�׏������t
           MOVE DT-BTCHJ-SHR-ST     IN YAB110
             TO DT-SHK-SHR          IN YF55204A
      *
      *  ���s��
           MOVE QA-NO-HKK           IN ZFL572A
             TO NO-HKK              IN YF55204A
      *
      *  �����v���R�[�h
           MOVE QF-REC-KBN          IN ZFL572A
             TO CD-TEISEI-FCT       IN YF55204A
      *
      *  �f�[�^�敪�������
           MOVE 'E'
             TO SCT-SALE-REPORT     IN YF55204A
      *
      *  ���[�敪
           MOVE 'E'
             TO SCT-REDISTER        IN YF55204A
      *
           .
       EDT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *         DPT-GET : �o�ו���R�[�h�擾�@�@�@�@�@�@               *
      ******************************************************************
       DPT-GET                 SECTION.
      *
      *  �m�a�g�a�s�P�T�R�n�i�ԏ�񌟍��i�����j
           MOVE SPACE
             TO YAB153
           MOVE ZERO
             TO WT-P-PS             IN GP-X201TB01 IN YAB153E2-ENT
             IN YAB153
           MOVE '01'
             TO QA-CD-PGMFUNC       IN YAB153E1-ENT IN YAB153
           MOVE HB-TR-Z             IN ZFL572A
             TO HB-TR-Z             IN YAB153E1-ENT IN YAB153
           MOVE DT-PAYOUT           IN ZFL572A
             TO DT-USE-ST           IN YAB153E1-ENT IN YAB153
           CALL 'ZAGB153' USING YAB153
           IF  QA-CD-RTN IN YAB153 = SPACE
      *  �m�a�g�a�s�P�P�P�n���B��񓯈�����f�[�^����
               MOVE SPACE
                 TO YAB111
               MOVE ZERO
                 TO QA-CTR-KENSU        IN YAB111E2-ENT IN YAB111
               PERFORM VARYING WK-CNV-G FROM 1 BY 1
                 UNTIL WK-CNV-G > 6
                   MOVE ZERO
                     TO RAT-CHT             IN GP-X202TB02
                     IN YAB111E3-ENT(WK-CNV-G)
               END-PERFORM
               PERFORM VARYING WK-CNV-G FROM 1 BY 1
                 UNTIL WK-CNV-G > 6
                   MOVE ZERO
                     TO NUM-CHT             IN GP-X202TB02
                     IN YAB111E3-ENT(WK-CNV-G)
               END-PERFORM
               MOVE '01'
                 TO QA-CD-PGMFUNC       IN YAB111E1-ENT IN YAB111
               MOVE HB-TR-Z             IN YAB153E2-ENT IN YAB153
                 TO HB-TR-Z             IN YAB111E1-ENT IN YAB111
               MOVE DT-PAYOUT           IN ZFL572A
                 TO DT-USE-ST           IN YAB111E1-ENT IN YAB111
               CALL 'ZAGB111' USING YAB111
               IF  QA-CD-RTN IN YAB111E2-ENT IN YAB111 = SPACE
      *  �m�a�g�a�s�P�U�T�n�o�ו���R�[�h�擾
                   MOVE SPACE
                     TO YAB165
                   MOVE '01'
                     TO QA-CD-PGMFUNC       IN YAB165
                   MOVE CD-WKCNTR           IN YAB111E3-ENT
                     IN YAB111(1)
                     TO CD-WKCNTR           IN YAB165
                   CALL 'ZAGB165' USING YAB165
                   IF  QA-CD-RTN IN YAB165 = SPACE
                       MOVE CD-SHK-DPT          IN YAB165
                         TO CD-SHK-DPT          IN YF55204A
                   END-IF
               END-IF
           END-IF
      *
      *  �o�ו���R�[�h���u�����N�̎��A���s���̓��Q�����Z�b�g
           IF  CD-SHK-DPT IN YF55204A = SPACE
               MOVE QA-NO-HKK           IN ZFL572A
                 TO WK-NO-HKK-2
               MOVE WK-NO-HKK-2
                 TO CD-SHK-DPT          IN YF55204A
           END-IF
      *
           .
       DPT-GET-EXIT.
           EXIT.
      ******************************************************************
      *         WRT-RTN : �o�͏����@�@�@�@�@�@�@�@�@�@�@               *
      ******************************************************************
       WRT-RTN                 SECTION.
      *
           WRITE YF55204A-REC FROM YF55204A
           END-WRITE
           COMPUTE WK-CTR-OUT = WK-CTR-OUT + 1
      *
           .
       WRT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *         END-RTN : �I�������@�@�@�@�@�@�@�@�@�@�@               *
      ******************************************************************
       END-RTN                 SECTION.
      *
      *-------- ���R�[�h�����\������
           PERFORM DSP-RTN THRU DSP-RTN-EXIT
      *-------- �t�@�C���E�N���[�Y����
           PERFORM CLS-FIL THRU CLS-FIL-EXIT
      *
           .
       END-RTN-EXIT.
           EXIT.
      ******************************************************************
      *         DSP-RTN : ���R�[�h�����\�������@�@�@�@�@               *
      ******************************************************************
       DSP-RTN                 SECTION.
      *
           MOVE SPACE
             TO YAG005
           PERFORM VARYING WK-CNV-G FROM 1 BY 1
             UNTIL WK-CNV-G > 20
               MOVE ZERO
                 TO QA-CTR-KENSU        IN YAG005E1-ENT(WK-CNV-G)
           END-PERFORM
           MOVE 'BGBB501'
             TO Q-NO-PGM            IN YAG005
           MOVE '0'
             TO QA-REV-PGM          IN YAG005
           MOVE 'IN-ZFL572A'
             TO QA-SV-KEY021        IN YAG005(1)
           MOVE 'OU-YF55204A'
             TO QA-SV-KEY021        IN YAG005(2)
           MOVE WK-CTR-INP
             TO QA-CTR-KENSU        IN YAG005(1)
           MOVE WK-CTR-OUT
             TO QA-CTR-KENSU        IN YAG005(2)
           CALL 'ZAEA301' USING YAG005
      *
           .
       DSP-RTN-EXIT.
           EXIT.
      ******************************************************************
      *         CLS-FIL : �t�@�C���E�N���[�Y�����@�@�@�@               *
      ******************************************************************
       CLS-FIL                 SECTION.
      *
           CLOSE ZFL572A-F
           CLOSE YF55204A-F
      *
           .
       CLS-FIL-EXIT.
           EXIT.

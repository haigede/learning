       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             BGBB501.
      * *************************************************************
      * **      プログラム名称                                    ***
      * **            未確定日々出荷実績データ編集　              ***
      * **      ＴＰモニター                                      ***
      * **                                                        ***
      * **      ＤＢＭＳ                                          ***
      * **                                                        ***
      * **      外部インターフェース                              ***
      * **                                                        ***
      * **      プログラム                                        ***
      * **            ＭＡＩＮ                                    ***
      * *************************************************************
      ******************************************************************
      *           IS
      *           　　　　［ジョブＩＤ：ＢＧＢＢ５］
      *           IL2
      *           Ｂ１１１：編集／全件データ編集
      *           （Ｖｅｒ　０１．００）
      *           IL5
      *           　　　　　＜＜＜　変更内容履歴管理　＞＞＞
      *           IL
      *           　　　　　　№　　　　　工事№　　　　　　　
      *           IL
      *           　　　　　　０　：　ＢＧＢＮ－００２９　　　
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
      *    ワーク変数エリア                                            *
      ******************************************************************
       01  WK-CNV-AREA.
         03  WK-CNV-AREA-START          PIC X(14)
             VALUE 'CNV-WORK-START'.
             COPY CBLCNS02.
         03  WK-CNV-AREA-END            PIC X(12)
             VALUE 'CNV-WORK-END'.
      ******************************************************************
      *                ワーク  エリア                                  *
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
      *-------- 初期処理
           PERFORM FST-RTN THRU FST-RTN-EXIT
      *-------- 入力処理
           PERFORM RED-RTN THRU RED-RTN-EXIT
           PERFORM
             UNTIL SW-EOF NOT = SPACE
      *-------- 編集処理
               PERFORM EDT-RTN THRU EDT-RTN-EXIT
      *-------- 出力処理
               PERFORM WRT-RTN THRU WRT-RTN-EXIT
      *-------- 入力処理
               PERFORM RED-RTN THRU RED-RTN-EXIT
           END-PERFORM
      *-------- 終了処理
           PERFORM END-RTN THRU END-RTN-EXIT
      *
           .
       MAIN-EXIT.
           GOBACK.
      ******************************************************************
      *         FST-RTN : 初期処理　　　　　　　　　　　               *
      ******************************************************************
       FST-RTN                 SECTION.
      *
      *-------- ファイル・オープン処理
           PERFORM OPN-FIL THRU OPN-FIL-EXIT
      *-------- クリア処理
           PERFORM INT-RTN THRU INT-RTN-EXIT
      *
      *  ［ＢＨＢＴ１１０］ＪＯＢ処理日付・時刻取得
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
      *         OPN-FIL : ファイル・オープン処理　　　　               *
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
      *         INT-RTN : クリア処理　　　　　　　　　　               *
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
      *         RED-RTN : 入力処理　　　　　　　　　　　               *
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
      *         EDT-RTN : 編集処理　　　　　　　　　　　               *
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
      *  出荷実績日付
           MOVE DT-PAYOUT           IN ZFL572A
             TO DT-SHK-JSK          IN YF55204A
      *
      *  出荷実績数
           IF  QA-SIGN IN ZFL572A ='-'
               COMPUTE KS-SHK-JSK IN YF55204A
                 = QA-KS-N8 IN ZFL572A * -1
             ELSE
               MOVE QA-KS-N8            IN ZFL572A
                 TO KS-SHK-JSK          IN YF55204A
           END-IF
      *
      *  発行番号
           MOVE QA-GKA-DPT-4        IN ZFL572A
             TO BG-HKK              IN YF55204A
      *
      *  出荷部門コード
      *-------- 出荷部門コード取得
           PERFORM DPT-GET THRU DPT-GET-EXIT
      *
      *  ＴＲ品番・システム
      *  ［ＢＨＢＴ０２１］ＴＲ品番システム変換サブ
           MOVE SPACE
             TO YAG018
           MOVE HB-TR-Z             IN ZFL572A
             TO QA-HB-TR            IN YAG018
           CALL 'ZAEB601' USING YAG018
           MOVE QA-HB-TR            IN YAG018
             TO HB-TR-Z-SYS         IN YF55204A
      *
      *  ＴＲ品番・設変無し
      *  ［ＢＨＢＴ０５１］ＴＲ品番・設変無し変換サブ
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
      *  提供データ作成ＪＯＢ
           MOVE 'BGBB5'
             TO CD-BT-TKY           IN YF55204A
      *
      *  出荷処理日付
           MOVE DT-BTCHJ-SHR-ST     IN YAB110
             TO DT-SHK-SHR          IN YF55204A
      *
      *  発行№
           MOVE QA-NO-HKK           IN ZFL572A
             TO NO-HKK              IN YF55204A
      *
      *  訂正要因コード
           MOVE QF-REC-KBN          IN ZFL572A
             TO CD-TEISEI-FCT       IN YF55204A
      *
      *  データ区分売上日報
           MOVE 'E'
             TO SCT-SALE-REPORT     IN YF55204A
      *
      *  帳票区分
           MOVE 'E'
             TO SCT-REDISTER        IN YF55204A
      *
           .
       EDT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *         DPT-GET : 出荷部門コード取得　　　　　　               *
      ******************************************************************
       DPT-GET                 SECTION.
      *
      *  ［ＢＨＢＴ１５３］品番情報検索（製造）
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
      *  ［ＢＨＢＴ１１１］調達情報同一日時データ検索
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
      *  ［ＢＨＢＴ１６５］出荷部門コード取得
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
      *  出荷部門コードがブランクの時、発行№の頭２桁をセット
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
      *         WRT-RTN : 出力処理　　　　　　　　　　　               *
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
      *         END-RTN : 終了処理　　　　　　　　　　　               *
      ******************************************************************
       END-RTN                 SECTION.
      *
      *-------- レコード件数表示処理
           PERFORM DSP-RTN THRU DSP-RTN-EXIT
      *-------- ファイル・クローズ処理
           PERFORM CLS-FIL THRU CLS-FIL-EXIT
      *
           .
       END-RTN-EXIT.
           EXIT.
      ******************************************************************
      *         DSP-RTN : レコード件数表示処理　　　　　               *
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
      *         CLS-FIL : ファイル・クローズ処理　　　　               *
      ******************************************************************
       CLS-FIL                 SECTION.
      *
           CLOSE ZFL572A-F
           CLOSE YF55204A-F
      *
           .
       CLS-FIL-EXIT.
           EXIT.

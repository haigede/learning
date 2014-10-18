#********************************************************************************
#    プロジェクト  :次期T-STAR
#    サブシステム  :
#    モジュール名  :Makefile
#    名称          :メイクファイル(基準系)
#
#    使用方法      :適宜修正して使用
#
#================================================================================
#  Copyright (C) 2006 by Nomura Research Institute,Ltd.
#  All rights reserved.
#********************************************************************************

#プログラムタイプ(SPP/BATCH/SUB/SLIB/CPY/DDL/SQL)
PGMTYPE		=	BATCH

#数値の18桁以上対応用フラグ(1:18桁以上対応、1以外：18桁以上未対応)
INTLENFLG	=	1

#適宜名前を修正する
LOADNAME	=	IXGS722

# ソースディレクトリなどは適宜追加・修正してください
BASEDIR		=	/home/apl/debug/GS
BASEDIRBL	=	/home/apl/BL
BASEDIRCMN	=	/eLIBSYS/T21/reference_space/develop/dev_RELSVNROOT/makefile/cmn
MKLOGDIR	=	/home/aplelib/debug/dev_RELSVNROOT/GS/IXGS722

# ORACLEユーザを適宜指定してください。
USERID		=	"/@utstar"
# APL 固有のライブラリがあれば記述してください
APLIB		=	

include  $(BASEDIRCMN)/KJN/MK/Makefile.common

# ------------------------------------------------------------------------------
# Load module difinition
# ------------------------------------------------------------------------------
LOAD		=	$(LOADNAME)
# ------------------------------------------------------------------------------
# Object module definition
# ------------------------------------------------------------------------------
# オブジェクトファイルを列挙してください
OBJS	=	IXGS722.o

# ------------------------------------------------------------------------------
# XA object definition
# ------------------------------------------------------------------------------
ifeq ($(PGMTYPE),SPP)
  XA_OBJ	=	$(TP1DIR)/spool/trnrmcmd/userobj/dc_trn_allrm.o
else
  XA_OBJ	=	
endif

# ------------------------------------------------------------------------------
# ターゲットの記述
# ------------------------------------------------------------------------------

include  $(BASEDIRCMN)/KJN/MK/Makefile.common2

include  $(BASEDIRCMN)/KJN/MK/Makefile.install

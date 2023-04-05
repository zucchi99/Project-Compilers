{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}

module Parser where

import ErrM
import LexGrammar
-- import qualified AbstractSyntax as AbsSyn
import AbstractSyntax
import qualified Types as Tipi
-- import AbsGrammar
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Ident)
	| HappyAbsSyn5 ((Int, (Int, Int)))
	| HappyAbsSyn6 ((Double, (Int, Int)))
	| HappyAbsSyn7 ((Char, (Int, Int)))
	| HappyAbsSyn8 ((String, (Int, Int)))
	| HappyAbsSyn9 ((Bool, (Int, Int)))
	| HappyAbsSyn10 (Program)
	| HappyAbsSyn11 (BlockWithDecl)
	| HappyAbsSyn12 ([Declaration])
	| HappyAbsSyn13 (BlockExec)
	| HappyAbsSyn14 ([Statement])
	| HappyAbsSyn15 ()
	| HappyAbsSyn19 (Declaration)
	| HappyAbsSyn24 (Maybe (RightExp))
	| HappyAbsSyn27 ([Ident])
	| HappyAbsSyn34 (Statement)
	| HappyAbsSyn36 ([RightExp])
	| HappyAbsSyn38 (Maybe ElseBlock)
	| HappyAbsSyn39 (Assign)
	| HappyAbsSyn40 (RightExp)
	| HappyAbsSyn48 (LeftExp)
	| HappyAbsSyn49 (Tipi.Type)
	| HappyAbsSyn52 ([(Int, Int)])
	| HappyAbsSyn53 ((Int, Int))
	| HappyAbsSyn54 (WritePrimitive)
	| HappyAbsSyn55 (ReadPrimitive)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,1003) ([0,0,0,0,0,0,4,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,32768,1028,16386,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,512,0,0,0,0,0,0,512,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,36872,6016,504,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,32,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1409,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,8192,10,8192,16,31748,0,0,0,128,0,0,0,0,0,0,0,2,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,128,30729,8065,0,0,0,41472,0,512,16385,1984,0,0,0,8,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,256,0,0,0,2592,0,4128,1024,124,0,0,0,512,0,0,0,0,0,0,0,0,0,0,64,0,0,0,32768,0,0,0,0,0,0,0,0,212,40964,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,212,40964,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,53,10241,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,2048,0,0,0,0,0,0,61440,1,0,0,0,0,0,40960,0,0,0,0,0,0,0,4128,32768,512,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24576,1,0,0,0,0,0,2592,0,4128,1024,124,0,0,32768,0,32768,0,61456,1,0,0,512,0,512,16384,1984,0,0,0,0,0,0,0,0,0,0,8192,0,8192,0,31748,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,648,0,1032,256,31,0,0,8192,10,8192,16,31748,0,0,0,10368,0,16512,4096,496,0,0,0,162,0,258,49216,7,0,0,0,0,256,16,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1024,0,0,0,0,0,0,1025,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,2592,0,4128,1024,124,0,0,0,0,0,0,0,0,0,0,41472,0,512,16385,1984,0,0,0,0,0,0,0,0,0,0,0,0,16416,24066,2016,0,0,0,0,0,0,0,0,0,0,0,162,0,258,49216,7,0,0,4096,0,0,0,0,0,0,0,1024,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,4096,0,0,0,0,8192,10,8192,16,31748,0,0,0,10368,0,16512,4096,496,0,0,0,0,512,57380,32261,0,0,0,4096,24576,1,0,0,0,0,0,64,1408,0,0,0,0,0,0,1,22,0,0,0,0,0,1024,22528,0,0,0,0,0,0,648,0,1032,256,31,0,0,0,0,16416,24066,2016,0,0,0,256,0,0,1,0,0,0,0,4,0,1024,0,0,0,0,4096,0,0,16,0,0,0,0,64,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,64,0,0,0,0,10368,0,16512,4096,496,0,0,0,162,0,258,49216,7,0,0,34816,2,2048,4,7937,0,0,0,2592,0,4128,1024,124,0,0,32768,40,32768,64,61456,1,0,0,41472,0,512,16385,1984,0,0,0,648,0,1032,256,31,0,0,8192,10,8192,16,31748,0,0,0,10368,0,16512,4096,496,0,0,0,162,0,258,49216,7,0,0,34816,2,2048,4,7937,0,0,0,2592,0,4128,1024,124,0,0,32768,40,32768,64,61456,1,0,0,41472,0,512,16385,1984,0,0,0,32768,0,0,0,0,0,0,0,0,212,40964,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41472,0,512,16385,1984,0,0,0,0,0,4096,0,0,0,0,0,32,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,31,0,0,0,0,0,0,10,0,0,0,0,0,0,10240,0,0,0,0,0,0,0,160,0,0,0,0,0,0,32768,2,0,0,0,0,0,0,2560,0,0,0,0,0,0,0,258,2048,32,0,0,0,0,2048,4,32800,0,0,0,0,0,64,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,16,1,0,0,0,0,0,0,0,0,0,0,0,34816,2,2048,4,7937,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9218,1504,126,0,0,0,0,0,0,0,0,0,0,0,0,16416,24066,2016,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,16396,2560,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","Ident","Integer","Double","Char","String","Boolean","Program","BlockWithDecl","ListDeclaration","BlockExec","ListStatement","NonMandatoryTerminator","Declaration","CostantsBlock","ListConstantDecl","ConstantDecl","VariablesBlock","ListVariableDeclBlock","ListVariableDeclFunc","VariableDeclBlock","InitAssign","DeclarationFunc","VariableDeclFunc","ListIdent","FunctionSign","ProcedureSign","FunctionDecl","ProcedureDecl","FunctionForw","ProcedureForw","FunctionCall","ProcedureCall","ListRightExp","Statement","ElseBlock","Assign","RightExp","RightExp1","RightExp2","RightExp3","RightExp4","RightExp5","RightExp6","RightExp7","LeftExp","Type","BaseType","CompositeType","ListArrayDeclarationDim","ArrayDeclarationDim","WritePrimitive","ReadPrimitive","'('","')'","'*'","'**'","'+'","','","'-'","'.'","'..'","'/'","':'","':='","';'","'<'","'<='","'='","'>'","'>='","'@'","'['","']'","'^'","'and'","'array'","'begin'","'boolean'","'char'","'const'","'div'","'do'","'else'","'end'","'false'","'for'","'forward'","'function'","'if'","'integer'","'mod'","'not'","'of'","'or'","'procedure'","'program'","'readChar'","'readInt'","'readReal'","'readString'","'real'","'repeat'","'string'","'then'","'to'","'true'","'until'","'var'","'while'","'writeChar'","'writeInt'","'writeReal'","'writeString'","L_ident","L_integ","L_doubl","L_charac","L_quoted","%eof"]
        bit_start = st * 122
        bit_end = (st + 1) * 122
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..121]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (99) = happyShift action_4
action_0 (10) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (117) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (122) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (117) = happyShift action_2
action_4 (4) = happyGoto action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (68) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (11) = happyGoto action_7
action_6 (12) = happyGoto action_8
action_6 _ = happyReduce_10

action_7 (63) = happyShift action_24
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (80) = happyShift action_19
action_8 (83) = happyShift action_20
action_8 (91) = happyShift action_21
action_8 (98) = happyShift action_22
action_8 (111) = happyShift action_23
action_8 (13) = happyGoto action_9
action_8 (16) = happyGoto action_10
action_8 (17) = happyGoto action_11
action_8 (20) = happyGoto action_12
action_8 (28) = happyGoto action_13
action_8 (29) = happyGoto action_14
action_8 (30) = happyGoto action_15
action_8 (31) = happyGoto action_16
action_8 (32) = happyGoto action_17
action_8 (33) = happyGoto action_18
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_9

action_10 _ = happyReduce_11

action_11 _ = happyReduce_18

action_12 _ = happyReduce_19

action_13 (90) = happyShift action_39
action_13 (11) = happyGoto action_38
action_13 (12) = happyGoto action_8
action_13 _ = happyReduce_10

action_14 (90) = happyShift action_37
action_14 (11) = happyGoto action_36
action_14 (12) = happyGoto action_8
action_14 _ = happyReduce_10

action_15 _ = happyReduce_22

action_16 _ = happyReduce_23

action_17 _ = happyReduce_20

action_18 _ = happyReduce_21

action_19 (68) = happyShift action_35
action_19 (15) = happyGoto action_34
action_19 _ = happyReduce_16

action_20 (117) = happyShift action_2
action_20 (4) = happyGoto action_31
action_20 (18) = happyGoto action_32
action_20 (19) = happyGoto action_33
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (117) = happyShift action_2
action_21 (4) = happyGoto action_30
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (117) = happyShift action_2
action_22 (4) = happyGoto action_29
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (117) = happyShift action_2
action_23 (4) = happyGoto action_25
action_23 (21) = happyGoto action_26
action_23 (23) = happyGoto action_27
action_23 (27) = happyGoto action_28
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_8

action_25 (61) = happyShift action_73
action_25 _ = happyReduce_40

action_26 _ = happyReduce_28

action_27 (68) = happyShift action_72
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (66) = happyShift action_71
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (56) = happyShift action_69
action_29 (25) = happyGoto action_70
action_29 _ = happyReduce_37

action_30 (56) = happyShift action_69
action_30 (25) = happyGoto action_68
action_30 _ = happyReduce_37

action_31 (71) = happyShift action_67
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_24

action_33 (68) = happyShift action_66
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (80) = happyShift action_19
action_34 (89) = happyShift action_54
action_34 (92) = happyShift action_55
action_34 (100) = happyShift action_56
action_34 (101) = happyShift action_57
action_34 (102) = happyShift action_58
action_34 (103) = happyShift action_59
action_34 (105) = happyShift action_60
action_34 (112) = happyShift action_61
action_34 (113) = happyShift action_62
action_34 (114) = happyShift action_63
action_34 (115) = happyShift action_64
action_34 (116) = happyShift action_65
action_34 (117) = happyShift action_2
action_34 (4) = happyGoto action_44
action_34 (13) = happyGoto action_45
action_34 (14) = happyGoto action_46
action_34 (34) = happyGoto action_47
action_34 (35) = happyGoto action_48
action_34 (37) = happyGoto action_49
action_34 (39) = happyGoto action_50
action_34 (48) = happyGoto action_51
action_34 (54) = happyGoto action_52
action_34 (55) = happyGoto action_53
action_34 _ = happyReduce_13

action_35 _ = happyReduce_17

action_36 (68) = happyShift action_43
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (68) = happyShift action_42
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (68) = happyShift action_41
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (68) = happyShift action_40
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_46

action_41 _ = happyReduce_44

action_42 _ = happyReduce_47

action_43 _ = happyReduce_45

action_44 (56) = happyShift action_137
action_44 _ = happyReduce_98

action_45 _ = happyReduce_53

action_46 (87) = happyShift action_136
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_59

action_48 _ = happyReduce_60

action_49 (68) = happyShift action_135
action_49 _ = happyReduce_14

action_50 _ = happyReduce_58

action_51 (67) = happyShift action_131
action_51 (74) = happyShift action_132
action_51 (75) = happyShift action_133
action_51 (77) = happyShift action_134
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_61

action_53 _ = happyReduce_62

action_54 (117) = happyShift action_2
action_54 (4) = happyGoto action_129
action_54 (39) = happyGoto action_130
action_54 (48) = happyGoto action_51
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (56) = happyShift action_107
action_55 (60) = happyShift action_108
action_55 (62) = happyShift action_109
action_55 (88) = happyShift action_110
action_55 (95) = happyShift action_111
action_55 (109) = happyShift action_112
action_55 (117) = happyShift action_2
action_55 (118) = happyShift action_113
action_55 (119) = happyShift action_114
action_55 (120) = happyShift action_115
action_55 (121) = happyShift action_116
action_55 (4) = happyGoto action_91
action_55 (5) = happyGoto action_92
action_55 (6) = happyGoto action_93
action_55 (7) = happyGoto action_94
action_55 (8) = happyGoto action_95
action_55 (9) = happyGoto action_96
action_55 (34) = happyGoto action_97
action_55 (40) = happyGoto action_128
action_55 (41) = happyGoto action_99
action_55 (42) = happyGoto action_100
action_55 (43) = happyGoto action_101
action_55 (44) = happyGoto action_102
action_55 (45) = happyGoto action_103
action_55 (46) = happyGoto action_104
action_55 (47) = happyGoto action_105
action_55 (48) = happyGoto action_106
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (56) = happyShift action_127
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (56) = happyShift action_126
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (56) = happyShift action_125
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (56) = happyShift action_124
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (80) = happyShift action_19
action_60 (89) = happyShift action_54
action_60 (92) = happyShift action_55
action_60 (100) = happyShift action_56
action_60 (101) = happyShift action_57
action_60 (102) = happyShift action_58
action_60 (103) = happyShift action_59
action_60 (105) = happyShift action_60
action_60 (112) = happyShift action_61
action_60 (113) = happyShift action_62
action_60 (114) = happyShift action_63
action_60 (115) = happyShift action_64
action_60 (116) = happyShift action_65
action_60 (117) = happyShift action_2
action_60 (4) = happyGoto action_44
action_60 (13) = happyGoto action_45
action_60 (34) = happyGoto action_47
action_60 (35) = happyGoto action_48
action_60 (37) = happyGoto action_123
action_60 (39) = happyGoto action_50
action_60 (48) = happyGoto action_51
action_60 (54) = happyGoto action_52
action_60 (55) = happyGoto action_53
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (56) = happyShift action_107
action_61 (60) = happyShift action_108
action_61 (62) = happyShift action_109
action_61 (88) = happyShift action_110
action_61 (95) = happyShift action_111
action_61 (109) = happyShift action_112
action_61 (117) = happyShift action_2
action_61 (118) = happyShift action_113
action_61 (119) = happyShift action_114
action_61 (120) = happyShift action_115
action_61 (121) = happyShift action_116
action_61 (4) = happyGoto action_91
action_61 (5) = happyGoto action_92
action_61 (6) = happyGoto action_93
action_61 (7) = happyGoto action_94
action_61 (8) = happyGoto action_95
action_61 (9) = happyGoto action_96
action_61 (34) = happyGoto action_97
action_61 (40) = happyGoto action_122
action_61 (41) = happyGoto action_99
action_61 (42) = happyGoto action_100
action_61 (43) = happyGoto action_101
action_61 (44) = happyGoto action_102
action_61 (45) = happyGoto action_103
action_61 (46) = happyGoto action_104
action_61 (47) = happyGoto action_105
action_61 (48) = happyGoto action_106
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (56) = happyShift action_121
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (56) = happyShift action_120
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (56) = happyShift action_119
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (56) = happyShift action_118
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (117) = happyShift action_2
action_66 (4) = happyGoto action_31
action_66 (18) = happyGoto action_117
action_66 (19) = happyGoto action_33
action_66 _ = happyReduce_25

action_67 (56) = happyShift action_107
action_67 (60) = happyShift action_108
action_67 (62) = happyShift action_109
action_67 (88) = happyShift action_110
action_67 (95) = happyShift action_111
action_67 (109) = happyShift action_112
action_67 (117) = happyShift action_2
action_67 (118) = happyShift action_113
action_67 (119) = happyShift action_114
action_67 (120) = happyShift action_115
action_67 (121) = happyShift action_116
action_67 (4) = happyGoto action_91
action_67 (5) = happyGoto action_92
action_67 (6) = happyGoto action_93
action_67 (7) = happyGoto action_94
action_67 (8) = happyGoto action_95
action_67 (9) = happyGoto action_96
action_67 (34) = happyGoto action_97
action_67 (40) = happyGoto action_98
action_67 (41) = happyGoto action_99
action_67 (42) = happyGoto action_100
action_67 (43) = happyGoto action_101
action_67 (44) = happyGoto action_102
action_67 (45) = happyGoto action_103
action_67 (46) = happyGoto action_104
action_67 (47) = happyGoto action_105
action_67 (48) = happyGoto action_106
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (66) = happyShift action_90
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (117) = happyShift action_2
action_69 (4) = happyGoto action_25
action_69 (22) = happyGoto action_87
action_69 (26) = happyGoto action_88
action_69 (27) = happyGoto action_89
action_69 _ = happyReduce_31

action_70 (68) = happyShift action_86
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (77) = happyShift action_79
action_71 (79) = happyShift action_80
action_71 (81) = happyShift action_81
action_71 (82) = happyShift action_82
action_71 (93) = happyShift action_83
action_71 (104) = happyShift action_84
action_71 (106) = happyShift action_85
action_71 (49) = happyGoto action_76
action_71 (50) = happyGoto action_77
action_71 (51) = happyGoto action_78
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (117) = happyShift action_2
action_72 (4) = happyGoto action_25
action_72 (21) = happyGoto action_75
action_72 (23) = happyGoto action_27
action_72 (27) = happyGoto action_28
action_72 _ = happyReduce_29

action_73 (117) = happyShift action_2
action_73 (4) = happyGoto action_25
action_73 (27) = happyGoto action_74
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_41

action_75 _ = happyReduce_30

action_76 (71) = happyShift action_181
action_76 (24) = happyGoto action_180
action_76 _ = happyReduce_35

action_77 _ = happyReduce_102

action_78 _ = happyReduce_103

action_79 (77) = happyShift action_79
action_79 (79) = happyShift action_80
action_79 (81) = happyShift action_81
action_79 (82) = happyShift action_82
action_79 (93) = happyShift action_83
action_79 (104) = happyShift action_84
action_79 (106) = happyShift action_85
action_79 (49) = happyGoto action_179
action_79 (50) = happyGoto action_77
action_79 (51) = happyGoto action_78
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (75) = happyShift action_178
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_107

action_82 _ = happyReduce_106

action_83 _ = happyReduce_104

action_84 _ = happyReduce_105

action_85 _ = happyReduce_108

action_86 _ = happyReduce_43

action_87 (57) = happyShift action_177
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (68) = happyShift action_176
action_88 _ = happyReduce_32

action_89 (66) = happyShift action_175
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (77) = happyShift action_79
action_90 (79) = happyShift action_80
action_90 (81) = happyShift action_81
action_90 (82) = happyShift action_82
action_90 (93) = happyShift action_83
action_90 (104) = happyShift action_84
action_90 (106) = happyShift action_85
action_90 (49) = happyGoto action_174
action_90 (50) = happyGoto action_77
action_90 (51) = happyGoto action_78
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (56) = happyShift action_173
action_91 _ = happyReduce_98

action_92 _ = happyReduce_91

action_93 _ = happyReduce_92

action_94 _ = happyReduce_93

action_95 _ = happyReduce_95

action_96 _ = happyReduce_94

action_97 _ = happyReduce_96

action_98 (97) = happyShift action_144
action_98 _ = happyReduce_27

action_99 (78) = happyShift action_172
action_99 _ = happyReduce_66

action_100 (69) = happyShift action_167
action_100 (70) = happyShift action_168
action_100 (71) = happyShift action_169
action_100 (72) = happyShift action_170
action_100 (73) = happyShift action_171
action_100 _ = happyReduce_68

action_101 (60) = happyShift action_165
action_101 (62) = happyShift action_166
action_101 _ = happyReduce_70

action_102 (58) = happyShift action_161
action_102 (65) = happyShift action_162
action_102 (84) = happyShift action_163
action_102 (94) = happyShift action_164
action_102 _ = happyReduce_76

action_103 (59) = happyShift action_160
action_103 _ = happyReduce_79

action_104 _ = happyReduce_84

action_105 _ = happyReduce_86

action_106 (74) = happyShift action_132
action_106 (75) = happyShift action_133
action_106 (77) = happyShift action_134
action_106 _ = happyReduce_97

action_107 (56) = happyShift action_107
action_107 (60) = happyShift action_108
action_107 (62) = happyShift action_109
action_107 (88) = happyShift action_110
action_107 (95) = happyShift action_111
action_107 (109) = happyShift action_112
action_107 (117) = happyShift action_2
action_107 (118) = happyShift action_113
action_107 (119) = happyShift action_114
action_107 (120) = happyShift action_115
action_107 (121) = happyShift action_116
action_107 (4) = happyGoto action_91
action_107 (5) = happyGoto action_92
action_107 (6) = happyGoto action_93
action_107 (7) = happyGoto action_94
action_107 (8) = happyGoto action_95
action_107 (9) = happyGoto action_96
action_107 (34) = happyGoto action_97
action_107 (40) = happyGoto action_159
action_107 (41) = happyGoto action_99
action_107 (42) = happyGoto action_100
action_107 (43) = happyGoto action_101
action_107 (44) = happyGoto action_102
action_107 (45) = happyGoto action_103
action_107 (46) = happyGoto action_104
action_107 (47) = happyGoto action_105
action_107 (48) = happyGoto action_106
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (56) = happyShift action_107
action_108 (88) = happyShift action_110
action_108 (109) = happyShift action_112
action_108 (117) = happyShift action_2
action_108 (118) = happyShift action_113
action_108 (119) = happyShift action_114
action_108 (120) = happyShift action_115
action_108 (121) = happyShift action_116
action_108 (4) = happyGoto action_91
action_108 (5) = happyGoto action_92
action_108 (6) = happyGoto action_93
action_108 (7) = happyGoto action_94
action_108 (8) = happyGoto action_95
action_108 (9) = happyGoto action_96
action_108 (34) = happyGoto action_97
action_108 (47) = happyGoto action_158
action_108 (48) = happyGoto action_106
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (56) = happyShift action_107
action_109 (88) = happyShift action_110
action_109 (109) = happyShift action_112
action_109 (117) = happyShift action_2
action_109 (118) = happyShift action_113
action_109 (119) = happyShift action_114
action_109 (120) = happyShift action_115
action_109 (121) = happyShift action_116
action_109 (4) = happyGoto action_91
action_109 (5) = happyGoto action_92
action_109 (6) = happyGoto action_93
action_109 (7) = happyGoto action_94
action_109 (8) = happyGoto action_95
action_109 (9) = happyGoto action_96
action_109 (34) = happyGoto action_97
action_109 (47) = happyGoto action_157
action_109 (48) = happyGoto action_106
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_7

action_111 (56) = happyShift action_107
action_111 (88) = happyShift action_110
action_111 (109) = happyShift action_112
action_111 (117) = happyShift action_2
action_111 (118) = happyShift action_113
action_111 (119) = happyShift action_114
action_111 (120) = happyShift action_115
action_111 (121) = happyShift action_116
action_111 (4) = happyGoto action_91
action_111 (5) = happyGoto action_92
action_111 (6) = happyGoto action_93
action_111 (7) = happyGoto action_94
action_111 (8) = happyGoto action_95
action_111 (9) = happyGoto action_96
action_111 (34) = happyGoto action_97
action_111 (47) = happyGoto action_156
action_111 (48) = happyGoto action_106
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_6

action_113 _ = happyReduce_2

action_114 _ = happyReduce_3

action_115 _ = happyReduce_4

action_116 _ = happyReduce_5

action_117 _ = happyReduce_26

action_118 (56) = happyShift action_107
action_118 (60) = happyShift action_108
action_118 (62) = happyShift action_109
action_118 (88) = happyShift action_110
action_118 (95) = happyShift action_111
action_118 (109) = happyShift action_112
action_118 (117) = happyShift action_2
action_118 (118) = happyShift action_113
action_118 (119) = happyShift action_114
action_118 (120) = happyShift action_115
action_118 (121) = happyShift action_116
action_118 (4) = happyGoto action_91
action_118 (5) = happyGoto action_92
action_118 (6) = happyGoto action_93
action_118 (7) = happyGoto action_94
action_118 (8) = happyGoto action_95
action_118 (9) = happyGoto action_96
action_118 (34) = happyGoto action_97
action_118 (40) = happyGoto action_155
action_118 (41) = happyGoto action_99
action_118 (42) = happyGoto action_100
action_118 (43) = happyGoto action_101
action_118 (44) = happyGoto action_102
action_118 (45) = happyGoto action_103
action_118 (46) = happyGoto action_104
action_118 (47) = happyGoto action_105
action_118 (48) = happyGoto action_106
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (56) = happyShift action_107
action_119 (60) = happyShift action_108
action_119 (62) = happyShift action_109
action_119 (88) = happyShift action_110
action_119 (95) = happyShift action_111
action_119 (109) = happyShift action_112
action_119 (117) = happyShift action_2
action_119 (118) = happyShift action_113
action_119 (119) = happyShift action_114
action_119 (120) = happyShift action_115
action_119 (121) = happyShift action_116
action_119 (4) = happyGoto action_91
action_119 (5) = happyGoto action_92
action_119 (6) = happyGoto action_93
action_119 (7) = happyGoto action_94
action_119 (8) = happyGoto action_95
action_119 (9) = happyGoto action_96
action_119 (34) = happyGoto action_97
action_119 (40) = happyGoto action_154
action_119 (41) = happyGoto action_99
action_119 (42) = happyGoto action_100
action_119 (43) = happyGoto action_101
action_119 (44) = happyGoto action_102
action_119 (45) = happyGoto action_103
action_119 (46) = happyGoto action_104
action_119 (47) = happyGoto action_105
action_119 (48) = happyGoto action_106
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (56) = happyShift action_107
action_120 (60) = happyShift action_108
action_120 (62) = happyShift action_109
action_120 (88) = happyShift action_110
action_120 (95) = happyShift action_111
action_120 (109) = happyShift action_112
action_120 (117) = happyShift action_2
action_120 (118) = happyShift action_113
action_120 (119) = happyShift action_114
action_120 (120) = happyShift action_115
action_120 (121) = happyShift action_116
action_120 (4) = happyGoto action_91
action_120 (5) = happyGoto action_92
action_120 (6) = happyGoto action_93
action_120 (7) = happyGoto action_94
action_120 (8) = happyGoto action_95
action_120 (9) = happyGoto action_96
action_120 (34) = happyGoto action_97
action_120 (40) = happyGoto action_153
action_120 (41) = happyGoto action_99
action_120 (42) = happyGoto action_100
action_120 (43) = happyGoto action_101
action_120 (44) = happyGoto action_102
action_120 (45) = happyGoto action_103
action_120 (46) = happyGoto action_104
action_120 (47) = happyGoto action_105
action_120 (48) = happyGoto action_106
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (56) = happyShift action_107
action_121 (60) = happyShift action_108
action_121 (62) = happyShift action_109
action_121 (88) = happyShift action_110
action_121 (95) = happyShift action_111
action_121 (109) = happyShift action_112
action_121 (117) = happyShift action_2
action_121 (118) = happyShift action_113
action_121 (119) = happyShift action_114
action_121 (120) = happyShift action_115
action_121 (121) = happyShift action_116
action_121 (4) = happyGoto action_91
action_121 (5) = happyGoto action_92
action_121 (6) = happyGoto action_93
action_121 (7) = happyGoto action_94
action_121 (8) = happyGoto action_95
action_121 (9) = happyGoto action_96
action_121 (34) = happyGoto action_97
action_121 (40) = happyGoto action_152
action_121 (41) = happyGoto action_99
action_121 (42) = happyGoto action_100
action_121 (43) = happyGoto action_101
action_121 (44) = happyGoto action_102
action_121 (45) = happyGoto action_103
action_121 (46) = happyGoto action_104
action_121 (47) = happyGoto action_105
action_121 (48) = happyGoto action_106
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (85) = happyShift action_151
action_122 (97) = happyShift action_144
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (110) = happyShift action_150
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (117) = happyShift action_2
action_124 (4) = happyGoto action_129
action_124 (48) = happyGoto action_149
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (117) = happyShift action_2
action_125 (4) = happyGoto action_129
action_125 (48) = happyGoto action_148
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (117) = happyShift action_2
action_126 (4) = happyGoto action_129
action_126 (48) = happyGoto action_147
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (117) = happyShift action_2
action_127 (4) = happyGoto action_129
action_127 (48) = happyGoto action_146
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (97) = happyShift action_144
action_128 (107) = happyShift action_145
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_98

action_130 (108) = happyShift action_143
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (56) = happyShift action_107
action_131 (60) = happyShift action_108
action_131 (62) = happyShift action_109
action_131 (88) = happyShift action_110
action_131 (95) = happyShift action_111
action_131 (109) = happyShift action_112
action_131 (117) = happyShift action_2
action_131 (118) = happyShift action_113
action_131 (119) = happyShift action_114
action_131 (120) = happyShift action_115
action_131 (121) = happyShift action_116
action_131 (4) = happyGoto action_91
action_131 (5) = happyGoto action_92
action_131 (6) = happyGoto action_93
action_131 (7) = happyGoto action_94
action_131 (8) = happyGoto action_95
action_131 (9) = happyGoto action_96
action_131 (34) = happyGoto action_97
action_131 (40) = happyGoto action_142
action_131 (41) = happyGoto action_99
action_131 (42) = happyGoto action_100
action_131 (43) = happyGoto action_101
action_131 (44) = happyGoto action_102
action_131 (45) = happyGoto action_103
action_131 (46) = happyGoto action_104
action_131 (47) = happyGoto action_105
action_131 (48) = happyGoto action_106
action_131 _ = happyFail (happyExpListPerState 131)

action_132 _ = happyReduce_101

action_133 (56) = happyShift action_107
action_133 (60) = happyShift action_108
action_133 (62) = happyShift action_109
action_133 (88) = happyShift action_110
action_133 (95) = happyShift action_111
action_133 (109) = happyShift action_112
action_133 (117) = happyShift action_2
action_133 (118) = happyShift action_113
action_133 (119) = happyShift action_114
action_133 (120) = happyShift action_115
action_133 (121) = happyShift action_116
action_133 (4) = happyGoto action_91
action_133 (5) = happyGoto action_92
action_133 (6) = happyGoto action_93
action_133 (7) = happyGoto action_94
action_133 (8) = happyGoto action_95
action_133 (9) = happyGoto action_96
action_133 (34) = happyGoto action_97
action_133 (36) = happyGoto action_141
action_133 (40) = happyGoto action_139
action_133 (41) = happyGoto action_99
action_133 (42) = happyGoto action_100
action_133 (43) = happyGoto action_101
action_133 (44) = happyGoto action_102
action_133 (45) = happyGoto action_103
action_133 (46) = happyGoto action_104
action_133 (47) = happyGoto action_105
action_133 (48) = happyGoto action_106
action_133 _ = happyReduce_50

action_134 _ = happyReduce_100

action_135 (80) = happyShift action_19
action_135 (89) = happyShift action_54
action_135 (92) = happyShift action_55
action_135 (100) = happyShift action_56
action_135 (101) = happyShift action_57
action_135 (102) = happyShift action_58
action_135 (103) = happyShift action_59
action_135 (105) = happyShift action_60
action_135 (112) = happyShift action_61
action_135 (113) = happyShift action_62
action_135 (114) = happyShift action_63
action_135 (115) = happyShift action_64
action_135 (116) = happyShift action_65
action_135 (117) = happyShift action_2
action_135 (4) = happyGoto action_44
action_135 (13) = happyGoto action_45
action_135 (14) = happyGoto action_140
action_135 (34) = happyGoto action_47
action_135 (35) = happyGoto action_48
action_135 (37) = happyGoto action_49
action_135 (39) = happyGoto action_50
action_135 (48) = happyGoto action_51
action_135 (54) = happyGoto action_52
action_135 (55) = happyGoto action_53
action_135 _ = happyReduce_13

action_136 _ = happyReduce_12

action_137 (56) = happyShift action_107
action_137 (60) = happyShift action_108
action_137 (62) = happyShift action_109
action_137 (88) = happyShift action_110
action_137 (95) = happyShift action_111
action_137 (109) = happyShift action_112
action_137 (117) = happyShift action_2
action_137 (118) = happyShift action_113
action_137 (119) = happyShift action_114
action_137 (120) = happyShift action_115
action_137 (121) = happyShift action_116
action_137 (4) = happyGoto action_91
action_137 (5) = happyGoto action_92
action_137 (6) = happyGoto action_93
action_137 (7) = happyGoto action_94
action_137 (8) = happyGoto action_95
action_137 (9) = happyGoto action_96
action_137 (34) = happyGoto action_97
action_137 (36) = happyGoto action_138
action_137 (40) = happyGoto action_139
action_137 (41) = happyGoto action_99
action_137 (42) = happyGoto action_100
action_137 (43) = happyGoto action_101
action_137 (44) = happyGoto action_102
action_137 (45) = happyGoto action_103
action_137 (46) = happyGoto action_104
action_137 (47) = happyGoto action_105
action_137 (48) = happyGoto action_106
action_137 _ = happyReduce_50

action_138 (57) = happyShift action_219
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (61) = happyShift action_218
action_139 (97) = happyShift action_144
action_139 _ = happyReduce_51

action_140 _ = happyReduce_15

action_141 (76) = happyShift action_217
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (97) = happyShift action_144
action_142 _ = happyReduce_65

action_143 (56) = happyShift action_107
action_143 (60) = happyShift action_108
action_143 (62) = happyShift action_109
action_143 (88) = happyShift action_110
action_143 (95) = happyShift action_111
action_143 (109) = happyShift action_112
action_143 (117) = happyShift action_2
action_143 (118) = happyShift action_113
action_143 (119) = happyShift action_114
action_143 (120) = happyShift action_115
action_143 (121) = happyShift action_116
action_143 (4) = happyGoto action_91
action_143 (5) = happyGoto action_92
action_143 (6) = happyGoto action_93
action_143 (7) = happyGoto action_94
action_143 (8) = happyGoto action_95
action_143 (9) = happyGoto action_96
action_143 (34) = happyGoto action_97
action_143 (40) = happyGoto action_216
action_143 (41) = happyGoto action_99
action_143 (42) = happyGoto action_100
action_143 (43) = happyGoto action_101
action_143 (44) = happyGoto action_102
action_143 (45) = happyGoto action_103
action_143 (46) = happyGoto action_104
action_143 (47) = happyGoto action_105
action_143 (48) = happyGoto action_106
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (56) = happyShift action_107
action_144 (60) = happyShift action_108
action_144 (62) = happyShift action_109
action_144 (88) = happyShift action_110
action_144 (95) = happyShift action_111
action_144 (109) = happyShift action_112
action_144 (117) = happyShift action_2
action_144 (118) = happyShift action_113
action_144 (119) = happyShift action_114
action_144 (120) = happyShift action_115
action_144 (121) = happyShift action_116
action_144 (4) = happyGoto action_91
action_144 (5) = happyGoto action_92
action_144 (6) = happyGoto action_93
action_144 (7) = happyGoto action_94
action_144 (8) = happyGoto action_95
action_144 (9) = happyGoto action_96
action_144 (34) = happyGoto action_97
action_144 (41) = happyGoto action_215
action_144 (42) = happyGoto action_100
action_144 (43) = happyGoto action_101
action_144 (44) = happyGoto action_102
action_144 (45) = happyGoto action_103
action_144 (46) = happyGoto action_104
action_144 (47) = happyGoto action_105
action_144 (48) = happyGoto action_106
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (80) = happyShift action_19
action_145 (89) = happyShift action_54
action_145 (92) = happyShift action_55
action_145 (100) = happyShift action_56
action_145 (101) = happyShift action_57
action_145 (102) = happyShift action_58
action_145 (103) = happyShift action_59
action_145 (105) = happyShift action_60
action_145 (112) = happyShift action_61
action_145 (113) = happyShift action_62
action_145 (114) = happyShift action_63
action_145 (115) = happyShift action_64
action_145 (116) = happyShift action_65
action_145 (117) = happyShift action_2
action_145 (4) = happyGoto action_44
action_145 (13) = happyGoto action_45
action_145 (34) = happyGoto action_47
action_145 (35) = happyGoto action_48
action_145 (37) = happyGoto action_214
action_145 (39) = happyGoto action_50
action_145 (48) = happyGoto action_51
action_145 (54) = happyGoto action_52
action_145 (55) = happyGoto action_53
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (57) = happyShift action_213
action_146 (74) = happyShift action_132
action_146 (75) = happyShift action_133
action_146 (77) = happyShift action_134
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (57) = happyShift action_212
action_147 (74) = happyShift action_132
action_147 (75) = happyShift action_133
action_147 (77) = happyShift action_134
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (57) = happyShift action_211
action_148 (74) = happyShift action_132
action_148 (75) = happyShift action_133
action_148 (77) = happyShift action_134
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (57) = happyShift action_210
action_149 (74) = happyShift action_132
action_149 (75) = happyShift action_133
action_149 (77) = happyShift action_134
action_149 _ = happyFail (happyExpListPerState 149)

action_150 (56) = happyShift action_107
action_150 (60) = happyShift action_108
action_150 (62) = happyShift action_109
action_150 (88) = happyShift action_110
action_150 (95) = happyShift action_111
action_150 (109) = happyShift action_112
action_150 (117) = happyShift action_2
action_150 (118) = happyShift action_113
action_150 (119) = happyShift action_114
action_150 (120) = happyShift action_115
action_150 (121) = happyShift action_116
action_150 (4) = happyGoto action_91
action_150 (5) = happyGoto action_92
action_150 (6) = happyGoto action_93
action_150 (7) = happyGoto action_94
action_150 (8) = happyGoto action_95
action_150 (9) = happyGoto action_96
action_150 (34) = happyGoto action_97
action_150 (40) = happyGoto action_209
action_150 (41) = happyGoto action_99
action_150 (42) = happyGoto action_100
action_150 (43) = happyGoto action_101
action_150 (44) = happyGoto action_102
action_150 (45) = happyGoto action_103
action_150 (46) = happyGoto action_104
action_150 (47) = happyGoto action_105
action_150 (48) = happyGoto action_106
action_150 _ = happyFail (happyExpListPerState 150)

action_151 (80) = happyShift action_19
action_151 (89) = happyShift action_54
action_151 (92) = happyShift action_55
action_151 (100) = happyShift action_56
action_151 (101) = happyShift action_57
action_151 (102) = happyShift action_58
action_151 (103) = happyShift action_59
action_151 (105) = happyShift action_60
action_151 (112) = happyShift action_61
action_151 (113) = happyShift action_62
action_151 (114) = happyShift action_63
action_151 (115) = happyShift action_64
action_151 (116) = happyShift action_65
action_151 (117) = happyShift action_2
action_151 (4) = happyGoto action_44
action_151 (13) = happyGoto action_45
action_151 (34) = happyGoto action_47
action_151 (35) = happyGoto action_48
action_151 (37) = happyGoto action_208
action_151 (39) = happyGoto action_50
action_151 (48) = happyGoto action_51
action_151 (54) = happyGoto action_52
action_151 (55) = happyGoto action_53
action_151 _ = happyFail (happyExpListPerState 151)

action_152 (57) = happyShift action_207
action_152 (97) = happyShift action_144
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (57) = happyShift action_206
action_153 (97) = happyShift action_144
action_153 _ = happyFail (happyExpListPerState 153)

action_154 (57) = happyShift action_205
action_154 (97) = happyShift action_144
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (57) = happyShift action_204
action_155 (97) = happyShift action_144
action_155 _ = happyFail (happyExpListPerState 155)

action_156 _ = happyReduce_87

action_157 _ = happyReduce_88

action_158 _ = happyReduce_89

action_159 (57) = happyShift action_203
action_159 (97) = happyShift action_144
action_159 _ = happyFail (happyExpListPerState 159)

action_160 (56) = happyShift action_107
action_160 (60) = happyShift action_108
action_160 (62) = happyShift action_109
action_160 (88) = happyShift action_110
action_160 (95) = happyShift action_111
action_160 (109) = happyShift action_112
action_160 (117) = happyShift action_2
action_160 (118) = happyShift action_113
action_160 (119) = happyShift action_114
action_160 (120) = happyShift action_115
action_160 (121) = happyShift action_116
action_160 (4) = happyGoto action_91
action_160 (5) = happyGoto action_92
action_160 (6) = happyGoto action_93
action_160 (7) = happyGoto action_94
action_160 (8) = happyGoto action_95
action_160 (9) = happyGoto action_96
action_160 (34) = happyGoto action_97
action_160 (46) = happyGoto action_202
action_160 (47) = happyGoto action_105
action_160 (48) = happyGoto action_106
action_160 _ = happyFail (happyExpListPerState 160)

action_161 (56) = happyShift action_107
action_161 (60) = happyShift action_108
action_161 (62) = happyShift action_109
action_161 (88) = happyShift action_110
action_161 (95) = happyShift action_111
action_161 (109) = happyShift action_112
action_161 (117) = happyShift action_2
action_161 (118) = happyShift action_113
action_161 (119) = happyShift action_114
action_161 (120) = happyShift action_115
action_161 (121) = happyShift action_116
action_161 (4) = happyGoto action_91
action_161 (5) = happyGoto action_92
action_161 (6) = happyGoto action_93
action_161 (7) = happyGoto action_94
action_161 (8) = happyGoto action_95
action_161 (9) = happyGoto action_96
action_161 (34) = happyGoto action_97
action_161 (45) = happyGoto action_201
action_161 (46) = happyGoto action_104
action_161 (47) = happyGoto action_105
action_161 (48) = happyGoto action_106
action_161 _ = happyFail (happyExpListPerState 161)

action_162 (56) = happyShift action_107
action_162 (60) = happyShift action_108
action_162 (62) = happyShift action_109
action_162 (88) = happyShift action_110
action_162 (95) = happyShift action_111
action_162 (109) = happyShift action_112
action_162 (117) = happyShift action_2
action_162 (118) = happyShift action_113
action_162 (119) = happyShift action_114
action_162 (120) = happyShift action_115
action_162 (121) = happyShift action_116
action_162 (4) = happyGoto action_91
action_162 (5) = happyGoto action_92
action_162 (6) = happyGoto action_93
action_162 (7) = happyGoto action_94
action_162 (8) = happyGoto action_95
action_162 (9) = happyGoto action_96
action_162 (34) = happyGoto action_97
action_162 (45) = happyGoto action_200
action_162 (46) = happyGoto action_104
action_162 (47) = happyGoto action_105
action_162 (48) = happyGoto action_106
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (56) = happyShift action_107
action_163 (60) = happyShift action_108
action_163 (62) = happyShift action_109
action_163 (88) = happyShift action_110
action_163 (95) = happyShift action_111
action_163 (109) = happyShift action_112
action_163 (117) = happyShift action_2
action_163 (118) = happyShift action_113
action_163 (119) = happyShift action_114
action_163 (120) = happyShift action_115
action_163 (121) = happyShift action_116
action_163 (4) = happyGoto action_91
action_163 (5) = happyGoto action_92
action_163 (6) = happyGoto action_93
action_163 (7) = happyGoto action_94
action_163 (8) = happyGoto action_95
action_163 (9) = happyGoto action_96
action_163 (34) = happyGoto action_97
action_163 (45) = happyGoto action_199
action_163 (46) = happyGoto action_104
action_163 (47) = happyGoto action_105
action_163 (48) = happyGoto action_106
action_163 _ = happyFail (happyExpListPerState 163)

action_164 (56) = happyShift action_107
action_164 (60) = happyShift action_108
action_164 (62) = happyShift action_109
action_164 (88) = happyShift action_110
action_164 (95) = happyShift action_111
action_164 (109) = happyShift action_112
action_164 (117) = happyShift action_2
action_164 (118) = happyShift action_113
action_164 (119) = happyShift action_114
action_164 (120) = happyShift action_115
action_164 (121) = happyShift action_116
action_164 (4) = happyGoto action_91
action_164 (5) = happyGoto action_92
action_164 (6) = happyGoto action_93
action_164 (7) = happyGoto action_94
action_164 (8) = happyGoto action_95
action_164 (9) = happyGoto action_96
action_164 (34) = happyGoto action_97
action_164 (45) = happyGoto action_198
action_164 (46) = happyGoto action_104
action_164 (47) = happyGoto action_105
action_164 (48) = happyGoto action_106
action_164 _ = happyFail (happyExpListPerState 164)

action_165 (56) = happyShift action_107
action_165 (60) = happyShift action_108
action_165 (62) = happyShift action_109
action_165 (88) = happyShift action_110
action_165 (95) = happyShift action_111
action_165 (109) = happyShift action_112
action_165 (117) = happyShift action_2
action_165 (118) = happyShift action_113
action_165 (119) = happyShift action_114
action_165 (120) = happyShift action_115
action_165 (121) = happyShift action_116
action_165 (4) = happyGoto action_91
action_165 (5) = happyGoto action_92
action_165 (6) = happyGoto action_93
action_165 (7) = happyGoto action_94
action_165 (8) = happyGoto action_95
action_165 (9) = happyGoto action_96
action_165 (34) = happyGoto action_97
action_165 (44) = happyGoto action_197
action_165 (45) = happyGoto action_103
action_165 (46) = happyGoto action_104
action_165 (47) = happyGoto action_105
action_165 (48) = happyGoto action_106
action_165 _ = happyFail (happyExpListPerState 165)

action_166 (56) = happyShift action_107
action_166 (60) = happyShift action_108
action_166 (62) = happyShift action_109
action_166 (88) = happyShift action_110
action_166 (95) = happyShift action_111
action_166 (109) = happyShift action_112
action_166 (117) = happyShift action_2
action_166 (118) = happyShift action_113
action_166 (119) = happyShift action_114
action_166 (120) = happyShift action_115
action_166 (121) = happyShift action_116
action_166 (4) = happyGoto action_91
action_166 (5) = happyGoto action_92
action_166 (6) = happyGoto action_93
action_166 (7) = happyGoto action_94
action_166 (8) = happyGoto action_95
action_166 (9) = happyGoto action_96
action_166 (34) = happyGoto action_97
action_166 (44) = happyGoto action_196
action_166 (45) = happyGoto action_103
action_166 (46) = happyGoto action_104
action_166 (47) = happyGoto action_105
action_166 (48) = happyGoto action_106
action_166 _ = happyFail (happyExpListPerState 166)

action_167 (56) = happyShift action_107
action_167 (60) = happyShift action_108
action_167 (62) = happyShift action_109
action_167 (88) = happyShift action_110
action_167 (95) = happyShift action_111
action_167 (109) = happyShift action_112
action_167 (117) = happyShift action_2
action_167 (118) = happyShift action_113
action_167 (119) = happyShift action_114
action_167 (120) = happyShift action_115
action_167 (121) = happyShift action_116
action_167 (4) = happyGoto action_91
action_167 (5) = happyGoto action_92
action_167 (6) = happyGoto action_93
action_167 (7) = happyGoto action_94
action_167 (8) = happyGoto action_95
action_167 (9) = happyGoto action_96
action_167 (34) = happyGoto action_97
action_167 (43) = happyGoto action_195
action_167 (44) = happyGoto action_102
action_167 (45) = happyGoto action_103
action_167 (46) = happyGoto action_104
action_167 (47) = happyGoto action_105
action_167 (48) = happyGoto action_106
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (56) = happyShift action_107
action_168 (60) = happyShift action_108
action_168 (62) = happyShift action_109
action_168 (88) = happyShift action_110
action_168 (95) = happyShift action_111
action_168 (109) = happyShift action_112
action_168 (117) = happyShift action_2
action_168 (118) = happyShift action_113
action_168 (119) = happyShift action_114
action_168 (120) = happyShift action_115
action_168 (121) = happyShift action_116
action_168 (4) = happyGoto action_91
action_168 (5) = happyGoto action_92
action_168 (6) = happyGoto action_93
action_168 (7) = happyGoto action_94
action_168 (8) = happyGoto action_95
action_168 (9) = happyGoto action_96
action_168 (34) = happyGoto action_97
action_168 (43) = happyGoto action_194
action_168 (44) = happyGoto action_102
action_168 (45) = happyGoto action_103
action_168 (46) = happyGoto action_104
action_168 (47) = happyGoto action_105
action_168 (48) = happyGoto action_106
action_168 _ = happyFail (happyExpListPerState 168)

action_169 (56) = happyShift action_107
action_169 (60) = happyShift action_108
action_169 (62) = happyShift action_109
action_169 (88) = happyShift action_110
action_169 (95) = happyShift action_111
action_169 (109) = happyShift action_112
action_169 (117) = happyShift action_2
action_169 (118) = happyShift action_113
action_169 (119) = happyShift action_114
action_169 (120) = happyShift action_115
action_169 (121) = happyShift action_116
action_169 (4) = happyGoto action_91
action_169 (5) = happyGoto action_92
action_169 (6) = happyGoto action_93
action_169 (7) = happyGoto action_94
action_169 (8) = happyGoto action_95
action_169 (9) = happyGoto action_96
action_169 (34) = happyGoto action_97
action_169 (43) = happyGoto action_193
action_169 (44) = happyGoto action_102
action_169 (45) = happyGoto action_103
action_169 (46) = happyGoto action_104
action_169 (47) = happyGoto action_105
action_169 (48) = happyGoto action_106
action_169 _ = happyFail (happyExpListPerState 169)

action_170 (56) = happyShift action_107
action_170 (60) = happyShift action_108
action_170 (62) = happyShift action_109
action_170 (88) = happyShift action_110
action_170 (95) = happyShift action_111
action_170 (109) = happyShift action_112
action_170 (117) = happyShift action_2
action_170 (118) = happyShift action_113
action_170 (119) = happyShift action_114
action_170 (120) = happyShift action_115
action_170 (121) = happyShift action_116
action_170 (4) = happyGoto action_91
action_170 (5) = happyGoto action_92
action_170 (6) = happyGoto action_93
action_170 (7) = happyGoto action_94
action_170 (8) = happyGoto action_95
action_170 (9) = happyGoto action_96
action_170 (34) = happyGoto action_97
action_170 (43) = happyGoto action_192
action_170 (44) = happyGoto action_102
action_170 (45) = happyGoto action_103
action_170 (46) = happyGoto action_104
action_170 (47) = happyGoto action_105
action_170 (48) = happyGoto action_106
action_170 _ = happyFail (happyExpListPerState 170)

action_171 (56) = happyShift action_107
action_171 (60) = happyShift action_108
action_171 (62) = happyShift action_109
action_171 (88) = happyShift action_110
action_171 (95) = happyShift action_111
action_171 (109) = happyShift action_112
action_171 (117) = happyShift action_2
action_171 (118) = happyShift action_113
action_171 (119) = happyShift action_114
action_171 (120) = happyShift action_115
action_171 (121) = happyShift action_116
action_171 (4) = happyGoto action_91
action_171 (5) = happyGoto action_92
action_171 (6) = happyGoto action_93
action_171 (7) = happyGoto action_94
action_171 (8) = happyGoto action_95
action_171 (9) = happyGoto action_96
action_171 (34) = happyGoto action_97
action_171 (43) = happyGoto action_191
action_171 (44) = happyGoto action_102
action_171 (45) = happyGoto action_103
action_171 (46) = happyGoto action_104
action_171 (47) = happyGoto action_105
action_171 (48) = happyGoto action_106
action_171 _ = happyFail (happyExpListPerState 171)

action_172 (56) = happyShift action_107
action_172 (60) = happyShift action_108
action_172 (62) = happyShift action_109
action_172 (88) = happyShift action_110
action_172 (95) = happyShift action_111
action_172 (109) = happyShift action_112
action_172 (117) = happyShift action_2
action_172 (118) = happyShift action_113
action_172 (119) = happyShift action_114
action_172 (120) = happyShift action_115
action_172 (121) = happyShift action_116
action_172 (4) = happyGoto action_91
action_172 (5) = happyGoto action_92
action_172 (6) = happyGoto action_93
action_172 (7) = happyGoto action_94
action_172 (8) = happyGoto action_95
action_172 (9) = happyGoto action_96
action_172 (34) = happyGoto action_97
action_172 (42) = happyGoto action_190
action_172 (43) = happyGoto action_101
action_172 (44) = happyGoto action_102
action_172 (45) = happyGoto action_103
action_172 (46) = happyGoto action_104
action_172 (47) = happyGoto action_105
action_172 (48) = happyGoto action_106
action_172 _ = happyFail (happyExpListPerState 172)

action_173 (56) = happyShift action_107
action_173 (60) = happyShift action_108
action_173 (62) = happyShift action_109
action_173 (88) = happyShift action_110
action_173 (95) = happyShift action_111
action_173 (109) = happyShift action_112
action_173 (117) = happyShift action_2
action_173 (118) = happyShift action_113
action_173 (119) = happyShift action_114
action_173 (120) = happyShift action_115
action_173 (121) = happyShift action_116
action_173 (4) = happyGoto action_91
action_173 (5) = happyGoto action_92
action_173 (6) = happyGoto action_93
action_173 (7) = happyGoto action_94
action_173 (8) = happyGoto action_95
action_173 (9) = happyGoto action_96
action_173 (34) = happyGoto action_97
action_173 (36) = happyGoto action_189
action_173 (40) = happyGoto action_139
action_173 (41) = happyGoto action_99
action_173 (42) = happyGoto action_100
action_173 (43) = happyGoto action_101
action_173 (44) = happyGoto action_102
action_173 (45) = happyGoto action_103
action_173 (46) = happyGoto action_104
action_173 (47) = happyGoto action_105
action_173 (48) = happyGoto action_106
action_173 _ = happyReduce_50

action_174 (68) = happyShift action_188
action_174 _ = happyFail (happyExpListPerState 174)

action_175 (77) = happyShift action_79
action_175 (79) = happyShift action_80
action_175 (81) = happyShift action_81
action_175 (82) = happyShift action_82
action_175 (93) = happyShift action_83
action_175 (104) = happyShift action_84
action_175 (106) = happyShift action_85
action_175 (49) = happyGoto action_187
action_175 (50) = happyGoto action_77
action_175 (51) = happyGoto action_78
action_175 _ = happyFail (happyExpListPerState 175)

action_176 (117) = happyShift action_2
action_176 (4) = happyGoto action_25
action_176 (22) = happyGoto action_186
action_176 (26) = happyGoto action_88
action_176 (27) = happyGoto action_89
action_176 _ = happyReduce_31

action_177 _ = happyReduce_38

action_178 (118) = happyShift action_113
action_178 (5) = happyGoto action_183
action_178 (52) = happyGoto action_184
action_178 (53) = happyGoto action_185
action_178 _ = happyReduce_111

action_179 _ = happyReduce_110

action_180 _ = happyReduce_34

action_181 (56) = happyShift action_107
action_181 (60) = happyShift action_108
action_181 (62) = happyShift action_109
action_181 (88) = happyShift action_110
action_181 (95) = happyShift action_111
action_181 (109) = happyShift action_112
action_181 (117) = happyShift action_2
action_181 (118) = happyShift action_113
action_181 (119) = happyShift action_114
action_181 (120) = happyShift action_115
action_181 (121) = happyShift action_116
action_181 (4) = happyGoto action_91
action_181 (5) = happyGoto action_92
action_181 (6) = happyGoto action_93
action_181 (7) = happyGoto action_94
action_181 (8) = happyGoto action_95
action_181 (9) = happyGoto action_96
action_181 (34) = happyGoto action_97
action_181 (40) = happyGoto action_182
action_181 (41) = happyGoto action_99
action_181 (42) = happyGoto action_100
action_181 (43) = happyGoto action_101
action_181 (44) = happyGoto action_102
action_181 (45) = happyGoto action_103
action_181 (46) = happyGoto action_104
action_181 (47) = happyGoto action_105
action_181 (48) = happyGoto action_106
action_181 _ = happyFail (happyExpListPerState 181)

action_182 (97) = happyShift action_144
action_182 _ = happyReduce_36

action_183 (64) = happyShift action_227
action_183 _ = happyFail (happyExpListPerState 183)

action_184 (76) = happyShift action_226
action_184 _ = happyFail (happyExpListPerState 184)

action_185 (61) = happyShift action_225
action_185 _ = happyReduce_112

action_186 _ = happyReduce_33

action_187 _ = happyReduce_39

action_188 _ = happyReduce_42

action_189 (57) = happyShift action_224
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (69) = happyShift action_167
action_190 (70) = happyShift action_168
action_190 (71) = happyShift action_169
action_190 (72) = happyShift action_170
action_190 (73) = happyShift action_171
action_190 _ = happyReduce_69

action_191 (60) = happyShift action_165
action_191 (62) = happyShift action_166
action_191 _ = happyReduce_73

action_192 (60) = happyShift action_165
action_192 (62) = happyShift action_166
action_192 _ = happyReduce_71

action_193 (60) = happyShift action_165
action_193 (62) = happyShift action_166
action_193 _ = happyReduce_75

action_194 (60) = happyShift action_165
action_194 (62) = happyShift action_166
action_194 _ = happyReduce_74

action_195 (60) = happyShift action_165
action_195 (62) = happyShift action_166
action_195 _ = happyReduce_72

action_196 (58) = happyShift action_161
action_196 (65) = happyShift action_162
action_196 (84) = happyShift action_163
action_196 (94) = happyShift action_164
action_196 _ = happyReduce_78

action_197 (58) = happyShift action_161
action_197 (65) = happyShift action_162
action_197 (84) = happyShift action_163
action_197 (94) = happyShift action_164
action_197 _ = happyReduce_77

action_198 (59) = happyShift action_160
action_198 _ = happyReduce_82

action_199 (59) = happyShift action_160
action_199 _ = happyReduce_83

action_200 (59) = happyShift action_160
action_200 _ = happyReduce_81

action_201 (59) = happyShift action_160
action_201 _ = happyReduce_80

action_202 _ = happyReduce_85

action_203 _ = happyReduce_90

action_204 _ = happyReduce_118

action_205 _ = happyReduce_116

action_206 _ = happyReduce_115

action_207 _ = happyReduce_117

action_208 _ = happyReduce_56

action_209 (97) = happyShift action_144
action_209 _ = happyReduce_57

action_210 _ = happyReduce_122

action_211 _ = happyReduce_120

action_212 _ = happyReduce_119

action_213 _ = happyReduce_121

action_214 (86) = happyShift action_223
action_214 (38) = happyGoto action_222
action_214 _ = happyReduce_63

action_215 (78) = happyShift action_172
action_215 _ = happyReduce_67

action_216 (85) = happyShift action_221
action_216 (97) = happyShift action_144
action_216 _ = happyFail (happyExpListPerState 216)

action_217 _ = happyReduce_99

action_218 (56) = happyShift action_107
action_218 (60) = happyShift action_108
action_218 (62) = happyShift action_109
action_218 (88) = happyShift action_110
action_218 (95) = happyShift action_111
action_218 (109) = happyShift action_112
action_218 (117) = happyShift action_2
action_218 (118) = happyShift action_113
action_218 (119) = happyShift action_114
action_218 (120) = happyShift action_115
action_218 (121) = happyShift action_116
action_218 (4) = happyGoto action_91
action_218 (5) = happyGoto action_92
action_218 (6) = happyGoto action_93
action_218 (7) = happyGoto action_94
action_218 (8) = happyGoto action_95
action_218 (9) = happyGoto action_96
action_218 (34) = happyGoto action_97
action_218 (36) = happyGoto action_220
action_218 (40) = happyGoto action_139
action_218 (41) = happyGoto action_99
action_218 (42) = happyGoto action_100
action_218 (43) = happyGoto action_101
action_218 (44) = happyGoto action_102
action_218 (45) = happyGoto action_103
action_218 (46) = happyGoto action_104
action_218 (47) = happyGoto action_105
action_218 (48) = happyGoto action_106
action_218 _ = happyReduce_50

action_219 (68) = happyReduce_49
action_219 (86) = happyReduce_49
action_219 (87) = happyReduce_49
action_219 (110) = happyReduce_49
action_219 _ = happyReduce_49

action_220 _ = happyReduce_52

action_221 (80) = happyShift action_19
action_221 (89) = happyShift action_54
action_221 (92) = happyShift action_55
action_221 (100) = happyShift action_56
action_221 (101) = happyShift action_57
action_221 (102) = happyShift action_58
action_221 (103) = happyShift action_59
action_221 (105) = happyShift action_60
action_221 (112) = happyShift action_61
action_221 (113) = happyShift action_62
action_221 (114) = happyShift action_63
action_221 (115) = happyShift action_64
action_221 (116) = happyShift action_65
action_221 (117) = happyShift action_2
action_221 (4) = happyGoto action_44
action_221 (13) = happyGoto action_45
action_221 (34) = happyGoto action_47
action_221 (35) = happyGoto action_48
action_221 (37) = happyGoto action_232
action_221 (39) = happyGoto action_50
action_221 (48) = happyGoto action_51
action_221 (54) = happyGoto action_52
action_221 (55) = happyGoto action_53
action_221 _ = happyFail (happyExpListPerState 221)

action_222 _ = happyReduce_54

action_223 (80) = happyShift action_19
action_223 (89) = happyShift action_54
action_223 (92) = happyShift action_55
action_223 (100) = happyShift action_56
action_223 (101) = happyShift action_57
action_223 (102) = happyShift action_58
action_223 (103) = happyShift action_59
action_223 (105) = happyShift action_60
action_223 (112) = happyShift action_61
action_223 (113) = happyShift action_62
action_223 (114) = happyShift action_63
action_223 (115) = happyShift action_64
action_223 (116) = happyShift action_65
action_223 (117) = happyShift action_2
action_223 (4) = happyGoto action_44
action_223 (13) = happyGoto action_45
action_223 (34) = happyGoto action_47
action_223 (35) = happyGoto action_48
action_223 (37) = happyGoto action_231
action_223 (39) = happyGoto action_50
action_223 (48) = happyGoto action_51
action_223 (54) = happyGoto action_52
action_223 (55) = happyGoto action_53
action_223 _ = happyFail (happyExpListPerState 223)

action_224 _ = happyReduce_48

action_225 (118) = happyShift action_113
action_225 (5) = happyGoto action_183
action_225 (52) = happyGoto action_230
action_225 (53) = happyGoto action_185
action_225 _ = happyReduce_111

action_226 (96) = happyShift action_229
action_226 _ = happyFail (happyExpListPerState 226)

action_227 (118) = happyShift action_113
action_227 (5) = happyGoto action_228
action_227 _ = happyFail (happyExpListPerState 227)

action_228 _ = happyReduce_114

action_229 (81) = happyShift action_81
action_229 (82) = happyShift action_82
action_229 (93) = happyShift action_83
action_229 (104) = happyShift action_84
action_229 (106) = happyShift action_85
action_229 (50) = happyGoto action_233
action_229 _ = happyFail (happyExpListPerState 229)

action_230 _ = happyReduce_113

action_231 _ = happyReduce_64

action_232 _ = happyReduce_55

action_233 _ = happyReduce_109

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (Ident {   id_name = (prToken happy_var_1),
                                ident_pos = (tokenLineCol happy_var_1)
                            }
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (((read (prToken happy_var_1)) :: Int, tokenLineCol happy_var_1)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (((read (prToken happy_var_1)) :: Double, tokenLineCol happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (((read (prToken happy_var_1)) :: Char, tokenLineCol happy_var_1)
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (((read (prToken happy_var_1)) :: String, tokenLineCol happy_var_1)
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 ((True, tokenLineCol happy_var_1)
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  9 happyReduction_7
happyReduction_7 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 ((False, tokenLineCol happy_var_1)
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 5 10 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (ProgramStart  {   program_name = happy_var_2,
                                                                    program_block_decl = happy_var_4,
                                                                    program_pos = (tokenLineCol happy_var_1)
                                                                }
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  11 happyReduction_9
happyReduction_9 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (BlockWithDeclaration  {   block_declarations = (reverse happy_var_1),
                                                                        block_exec = happy_var_2,
                                                                        block_with_decl_pos = if (null happy_var_1) then (block_exec_pos happy_var_2) else (declaration_pos (head happy_var_1))
                                                                    }
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  12 happyReduction_10
happyReduction_10  =  HappyAbsSyn12
		 ([]
	)

happyReduce_11 = happySpecReduce_2  12 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_2 ++ happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 13 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (BlockOnlyExecution    {   statements = happy_var_3,
                                                                                                block_exec_pos = (tokenLineCol happy_var_1)
                                                                                            }
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_0  14 happyReduction_13
happyReduction_13  =  HappyAbsSyn14
		 ([]
	)

happyReduce_14 = happySpecReduce_1  14 happyReduction_14
happyReduction_14 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn14
		 ((:[]) happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  14 happyReduction_15
happyReduction_15 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn14
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_0  15 happyReduction_16
happyReduction_16  =  HappyAbsSyn15
		 (
	)

happyReduce_17 = happySpecReduce_1  15 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn15
		 (
	)

happyReduce_18 = happySpecReduce_1  16 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  16 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  16 happyReduction_20
happyReduction_20 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  16 happyReduction_21
happyReduction_21 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  16 happyReduction_22
happyReduction_22 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  16 happyReduction_23
happyReduction_23 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  17 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  18 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn12
		 ((:[]) happy_var_1
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  18 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn12
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  19 happyReduction_27
happyReduction_27 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn19
		 (DeclarationCostant   {   costant_name = happy_var_1,
                                                                costant_type_maybe = Nothing,
                                                                costant_value = happy_var_3,
                                                                declaration_pos = (ident_pos happy_var_1)
                                                            }
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  20 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  21 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  21 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0  22 happyReduction_31
happyReduction_31  =  HappyAbsSyn12
		 ([]
	)

happyReduce_32 = happySpecReduce_1  22 happyReduction_32
happyReduction_32 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  22 happyReduction_33
happyReduction_33 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happyReduce 4 23 happyReduction_34
happyReduction_34 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	(HappyAbsSyn49  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (-- foreach element in ListIdent, create a DeclarationVariable
    let createDeclarationVariable :: Ident -> Declaration
        createDeclarationVariable ident = DeclarationVariable   {   variable_name = ident,
                                                                    variable_type = happy_var_3,
                                                                    variable_value_maybe = happy_var_4,
                                                                    declaration_pos = (ident_pos ident)
                                                                } 
    in map (createDeclarationVariable) happy_var_1
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_0  24 happyReduction_35
happyReduction_35  =  HappyAbsSyn24
		 (Nothing
	)

happyReduce_36 = happySpecReduce_2  24 happyReduction_36
happyReduction_36 (HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Just happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  25 happyReduction_37
happyReduction_37  =  HappyAbsSyn12
		 ([]
	)

happyReduce_38 = happySpecReduce_3  25 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  26 happyReduction_39
happyReduction_39 (HappyAbsSyn49  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn12
		 (-- foreach element in ListIdent, create a DeclarationVariable
    let createDeclarationVariable :: Ident -> Declaration
        createDeclarationVariable ident = DeclarationVariable   {   variable_name = ident,
                                                                    variable_type = happy_var_3,
                                                                    variable_value_maybe = Nothing,
                                                                    declaration_pos = (ident_pos ident)
                                                                } 
    in map (createDeclarationVariable) happy_var_1
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  27 happyReduction_40
happyReduction_40 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn27
		 ((:[]) happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  27 happyReduction_41
happyReduction_41 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn27
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happyReduce 6 28 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn49  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (DeclarationFunction   {   declaration_name = happy_var_2,
                                                                                            declaration_params = happy_var_3,
                                                                                            function_type = happy_var_5,
                                                                                            declaration_body_maybe = Nothing,
                                                                                            declaration_pos = (tokenLineCol happy_var_1)
                                                                                        }
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 4 29 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (DeclarationProcedure  {   declaration_name = happy_var_2,
                                                                                    declaration_params = happy_var_3,
                                                                                    declaration_body_maybe = Nothing,
                                                                                    declaration_pos = (tokenLineCol happy_var_1)
                                                                                }
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_3  30 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (DeclarationFunction   {   declaration_name = declaration_name happy_var_1,
                                                                                declaration_params = declaration_params happy_var_1,
                                                                                function_type = function_type happy_var_1,
                                                                                declaration_body_maybe = Just happy_var_2,
                                                                                declaration_pos = (declaration_pos happy_var_1)
                                                                            }
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  31 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (DeclarationProcedure  {   declaration_name = declaration_name happy_var_1,
                                                                                declaration_params = declaration_params happy_var_1,
                                                                                declaration_body_maybe = Just happy_var_2,
                                                                                declaration_pos = (declaration_pos happy_var_1)
                                                                            }
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  32 happyReduction_46
happyReduction_46 _
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  33 happyReduction_47
happyReduction_47 _
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 4 34 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (StatementFunctionCall {   call_name = happy_var_1,
                                                                            call_params = happy_var_3,
                                                                            statement_pos = (ident_pos happy_var_1)
                                                                        }
	) `HappyStk` happyRest

happyReduce_49 = happyReduce 4 35 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (StatementProcedureCall    {   call_name = happy_var_1,
                                                                                call_params = happy_var_3,
                                                                                statement_pos = (ident_pos happy_var_1)
                                                                            }
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_0  36 happyReduction_50
happyReduction_50  =  HappyAbsSyn36
		 ([]
	)

happyReduce_51 = happySpecReduce_1  36 happyReduction_51
happyReduction_51 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn36
		 ((:[]) happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  36 happyReduction_52
happyReduction_52 (HappyAbsSyn36  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn36
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  37 happyReduction_53
happyReduction_53 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn34
		 (StatementBlock    {   block = happy_var_1,
                                                                                    statement_pos = (block_exec_pos happy_var_1)
                                                                                }
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happyReduce 5 37 happyReduction_54
happyReduction_54 ((HappyAbsSyn38  happy_var_5) `HappyStk`
	(HappyAbsSyn34  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (StatementIf   {   condition = happy_var_2,
                                                                                then_body = happy_var_4,
                                                                                else_body_maybe = happy_var_5,
                                                                                statement_pos = (tokenLineCol happy_var_1)
                                                                            }
	) `HappyStk` happyRest

happyReduce_55 = happyReduce 6 37 happyReduction_55
happyReduction_55 ((HappyAbsSyn34  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (StatementFor  {   condition = happy_var_4,
                                                                                then_body = happy_var_6,
                                                                                for_var = happy_var_2,
                                                                                statement_pos = (tokenLineCol happy_var_1)
                                                                            }
	) `HappyStk` happyRest

happyReduce_56 = happyReduce 4 37 happyReduction_56
happyReduction_56 ((HappyAbsSyn34  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (StatementWhile    {   condition = happy_var_2,
                                                                                    then_body = happy_var_4,
                                                                                    statement_pos = (tokenLineCol happy_var_1)
                                                                                }
	) `HappyStk` happyRest

happyReduce_57 = happyReduce 4 37 happyReduction_57
happyReduction_57 ((HappyAbsSyn40  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (StatementRepeatUntil  {   condition = happy_var_4,
                                                                                        then_body = happy_var_2,
                                                                                        statement_pos = (tokenLineCol happy_var_1)
                                                                                    }
	) `HappyStk` happyRest

happyReduce_58 = happySpecReduce_1  37 happyReduction_58
happyReduction_58 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn34
		 (StatementAssign   {   assign = happy_var_1,
                                                                                    statement_pos = (assign_pos happy_var_1)
                                                                                }
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  37 happyReduction_59
happyReduction_59 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  37 happyReduction_60
happyReduction_60 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  37 happyReduction_61
happyReduction_61 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn34
		 (StatementWrite    {   write_primitive = happy_var_1,
                                                                                    statement_pos = (write_primitive_pos happy_var_1)
                                                                                }
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  37 happyReduction_62
happyReduction_62 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn34
		 (StatementRead     {   read_primitive = happy_var_1,
                                                                                    statement_pos = (read_primitive_pos happy_var_1)
                                                                                }
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_0  38 happyReduction_63
happyReduction_63  =  HappyAbsSyn38
		 (Nothing
	)

happyReduce_64 = happySpecReduce_2  38 happyReduction_64
happyReduction_64 (HappyAbsSyn34  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn38
		 (Just ElseBlock    {   else_body = happy_var_2,
                                                                                    else_block_pos = (tokenLineCol happy_var_1)
                                                                                }
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  39 happyReduction_65
happyReduction_65 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn39
		 (VariableAssignment    {   left_exp_assignment = happy_var_1,
                                                                right_exp_assignment = happy_var_3,
                                                                assign_pos = (left_exp_pos happy_var_1)
                                                            }
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  40 happyReduction_66
happyReduction_66 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  40 happyReduction_67
happyReduction_67 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpOr    {   sx = happy_var_1,
                                                                    dx = happy_var_3,
                                                                    right_exp_pos = (right_exp_pos happy_var_1)
                                                                }
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  41 happyReduction_68
happyReduction_68 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  41 happyReduction_69
happyReduction_69 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpAnd { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  42 happyReduction_70
happyReduction_70 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  42 happyReduction_71
happyReduction_71 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpGreater { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1)}
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  42 happyReduction_72
happyReduction_72 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpLess { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  42 happyReduction_73
happyReduction_73 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpGreaterEqual { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  42 happyReduction_74
happyReduction_74 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpLessEqual { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  42 happyReduction_75
happyReduction_75 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpEqual { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  43 happyReduction_76
happyReduction_76 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  43 happyReduction_77
happyReduction_77 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpPlus { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  43 happyReduction_78
happyReduction_78 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpMinus { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  44 happyReduction_79
happyReduction_79 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  44 happyReduction_80
happyReduction_80 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpTimes { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  44 happyReduction_81
happyReduction_81 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpDivide { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  44 happyReduction_82
happyReduction_82 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpMod { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_3  44 happyReduction_83
happyReduction_83 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpDiv { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  45 happyReduction_84
happyReduction_84 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_3  45 happyReduction_85
happyReduction_85 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpPower { sx = happy_var_1, dx = happy_var_3, right_exp_pos = (right_exp_pos happy_var_1) }
	)
happyReduction_85 _ _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  46 happyReduction_86
happyReduction_86 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_2  46 happyReduction_87
happyReduction_87 (HappyAbsSyn40  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpNot { dx = happy_var_2, right_exp_pos = (tokenLineCol happy_var_1) }
	)
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_2  46 happyReduction_88
happyReduction_88 (HappyAbsSyn40  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpMinusUnary { dx = happy_var_2, right_exp_pos = (tokenLineCol happy_var_1) }
	)
happyReduction_88 _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_2  46 happyReduction_89
happyReduction_89 (HappyAbsSyn40  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpPlusUnary { dx = happy_var_2, right_exp_pos = (tokenLineCol happy_var_1) }
	)
happyReduction_89 _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_3  47 happyReduction_90
happyReduction_90 _
	(HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn40
		 (happy_var_2
	)
happyReduction_90 _ _ _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  47 happyReduction_91
happyReduction_91 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpInteger   {   right_exp_int = fst happy_var_1,
                                                                        right_exp_pos = snd happy_var_1
                                                                    }
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  47 happyReduction_92
happyReduction_92 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpReal      {   right_exp_double = fst happy_var_1,
                                                                        right_exp_pos = snd happy_var_1
                                                                    }
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  47 happyReduction_93
happyReduction_93 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpChar      {   right_exp_char = fst happy_var_1,
                                                                        right_exp_pos = snd happy_var_1
                                                                    }
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  47 happyReduction_94
happyReduction_94 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpBoolean   {   right_exp_bool = fst happy_var_1,
                                                                        right_exp_pos = snd happy_var_1
                                                                    }
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  47 happyReduction_95
happyReduction_95 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpString    {   right_exp_string = fst happy_var_1,
                                                                        right_exp_pos = snd happy_var_1
                                                                    }
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  47 happyReduction_96
happyReduction_96 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpFunctionCall  {   call_name_right_exp = call_name happy_var_1,
                                                                            call_params_right_exp = call_params happy_var_1,
                                                                            right_exp_pos = (statement_pos happy_var_1) 
                                                                        }
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  47 happyReduction_97
happyReduction_97 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn40
		 (RightExpCopy      {   left_exp_right_exp = happy_var_1, 
                                                                        right_exp_pos = (left_exp_pos happy_var_1)
                                                                    }
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_1  48 happyReduction_98
happyReduction_98 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn48
		 (LeftExpIdent { left_exp_name = happy_var_1, left_exp_pos = (ident_pos happy_var_1) }
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happyReduce 4 48 happyReduction_99
happyReduction_99 (_ `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn48
		 (LeftExpArrayAccess { array_name = happy_var_1, array_pos = happy_var_3, left_exp_pos = (left_exp_pos happy_var_1) }
	) `HappyStk` happyRest

happyReduce_100 = happySpecReduce_2  48 happyReduction_100
happyReduction_100 _
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (LeftExpPointerValue { pointer_value = happy_var_1, left_exp_pos = (left_exp_pos happy_var_1) }
	)
happyReduction_100 _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_2  48 happyReduction_101
happyReduction_101 _
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (LeftExpPointerAddress { pointer_address = happy_var_1, left_exp_pos = (left_exp_pos happy_var_1) }
	)
happyReduction_101 _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  49 happyReduction_102
happyReduction_102 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  49 happyReduction_103
happyReduction_103 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_1  50 happyReduction_104
happyReduction_104 _
	 =  HappyAbsSyn49
		 (Tipi.IntegerType
	)

happyReduce_105 = happySpecReduce_1  50 happyReduction_105
happyReduction_105 _
	 =  HappyAbsSyn49
		 (Tipi.RealType
	)

happyReduce_106 = happySpecReduce_1  50 happyReduction_106
happyReduction_106 _
	 =  HappyAbsSyn49
		 (Tipi.CharType
	)

happyReduce_107 = happySpecReduce_1  50 happyReduction_107
happyReduction_107 _
	 =  HappyAbsSyn49
		 (Tipi.BooleanType
	)

happyReduce_108 = happySpecReduce_1  50 happyReduction_108
happyReduction_108 _
	 =  HappyAbsSyn49
		 (Tipi.StringType
	)

happyReduce_109 = happyReduce 6 51 happyReduction_109
happyReduction_109 ((HappyAbsSyn49  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn49
		 (Tipi.ArrayType { Tipi.aType = happy_var_6, Tipi.dimensions = happy_var_3 }
	) `HappyStk` happyRest

happyReduce_110 = happySpecReduce_2  51 happyReduction_110
happyReduction_110 (HappyAbsSyn49  happy_var_2)
	_
	 =  HappyAbsSyn49
		 (Tipi.PointerType { Tipi.pType = happy_var_2 }
	)
happyReduction_110 _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_0  52 happyReduction_111
happyReduction_111  =  HappyAbsSyn52
		 ([]
	)

happyReduce_112 = happySpecReduce_1  52 happyReduction_112
happyReduction_112 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn52
		 ((:[]) happy_var_1
	)
happyReduction_112 _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_3  52 happyReduction_113
happyReduction_113 (HappyAbsSyn52  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn52
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_113 _ _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_3  53 happyReduction_114
happyReduction_114 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn53
		 ((fst happy_var_1, fst happy_var_3)
	)
happyReduction_114 _ _ _  = notHappyAtAll 

happyReduce_115 = happyReduce 4 54 happyReduction_115
happyReduction_115 (_ `HappyStk`
	(HappyAbsSyn40  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 (WriteInt  {   write_exp = happy_var_3,
                                                                    write_primitive_pos = (tokenLineCol happy_var_1)
                                                                }
	) `HappyStk` happyRest

happyReduce_116 = happyReduce 4 54 happyReduction_116
happyReduction_116 (_ `HappyStk`
	(HappyAbsSyn40  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 (WriteReal {   write_exp = happy_var_3,
                                                                    write_primitive_pos = (tokenLineCol happy_var_1)
                                                                }
	) `HappyStk` happyRest

happyReduce_117 = happyReduce 4 54 happyReduction_117
happyReduction_117 (_ `HappyStk`
	(HappyAbsSyn40  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 (WriteChar {   write_exp = happy_var_3,
                                                                    write_primitive_pos = (tokenLineCol happy_var_1)
                                                                }
	) `HappyStk` happyRest

happyReduce_118 = happyReduce 4 54 happyReduction_118
happyReduction_118 (_ `HappyStk`
	(HappyAbsSyn40  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 (WriteString { write_exp = happy_var_3,
                                                                    write_primitive_pos = (tokenLineCol happy_var_1)
                                                                }
	) `HappyStk` happyRest

happyReduce_119 = happyReduce 4 55 happyReduction_119
happyReduction_119 (_ `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn55
		 (ReadInt       {   read_exp = happy_var_3,
                                                                    read_primitive_pos = (tokenLineCol happy_var_1)
                                                                }
	) `HappyStk` happyRest

happyReduce_120 = happyReduce 4 55 happyReduction_120
happyReduction_120 (_ `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn55
		 (ReadReal      {   read_exp = happy_var_3,
                                                                    read_primitive_pos = (tokenLineCol happy_var_1)
                                                                }
	) `HappyStk` happyRest

happyReduce_121 = happyReduce 4 55 happyReduction_121
happyReduction_121 (_ `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn55
		 (ReadChar      {   read_exp = happy_var_3,
                                                                    read_primitive_pos = (tokenLineCol happy_var_1)
                                                                }
	) `HappyStk` happyRest

happyReduce_122 = happyReduce 4 55 happyReduction_122
happyReduction_122 (_ `HappyStk`
	(HappyAbsSyn48  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn55
		 (ReadString    {   read_exp = happy_var_3,
                                                                    read_primitive_pos = (tokenLineCol happy_var_1)
                                                                }
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 122 122 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 56;
	PT _ (TS _ 2) -> cont 57;
	PT _ (TS _ 3) -> cont 58;
	PT _ (TS _ 4) -> cont 59;
	PT _ (TS _ 5) -> cont 60;
	PT _ (TS _ 6) -> cont 61;
	PT _ (TS _ 7) -> cont 62;
	PT _ (TS _ 8) -> cont 63;
	PT _ (TS _ 9) -> cont 64;
	PT _ (TS _ 10) -> cont 65;
	PT _ (TS _ 11) -> cont 66;
	PT _ (TS _ 12) -> cont 67;
	PT _ (TS _ 13) -> cont 68;
	PT _ (TS _ 14) -> cont 69;
	PT _ (TS _ 15) -> cont 70;
	PT _ (TS _ 16) -> cont 71;
	PT _ (TS _ 17) -> cont 72;
	PT _ (TS _ 18) -> cont 73;
	PT _ (TS _ 19) -> cont 74;
	PT _ (TS _ 20) -> cont 75;
	PT _ (TS _ 21) -> cont 76;
	PT _ (TS _ 22) -> cont 77;
	PT _ (TS _ 23) -> cont 78;
	PT _ (TS _ 24) -> cont 79;
	PT _ (TS _ 25) -> cont 80;
	PT _ (TS _ 26) -> cont 81;
	PT _ (TS _ 27) -> cont 82;
	PT _ (TS _ 28) -> cont 83;
	PT _ (TS _ 29) -> cont 84;
	PT _ (TS _ 30) -> cont 85;
	PT _ (TS _ 31) -> cont 86;
	PT _ (TS _ 32) -> cont 87;
	PT _ (TS _ 33) -> cont 88;
	PT _ (TS _ 34) -> cont 89;
	PT _ (TS _ 35) -> cont 90;
	PT _ (TS _ 36) -> cont 91;
	PT _ (TS _ 37) -> cont 92;
	PT _ (TS _ 38) -> cont 93;
	PT _ (TS _ 39) -> cont 94;
	PT _ (TS _ 40) -> cont 95;
	PT _ (TS _ 41) -> cont 96;
	PT _ (TS _ 42) -> cont 97;
	PT _ (TS _ 43) -> cont 98;
	PT _ (TS _ 44) -> cont 99;
	PT _ (TS _ 45) -> cont 100;
	PT _ (TS _ 46) -> cont 101;
	PT _ (TS _ 47) -> cont 102;
	PT _ (TS _ 48) -> cont 103;
	PT _ (TS _ 49) -> cont 104;
	PT _ (TS _ 50) -> cont 105;
	PT _ (TS _ 51) -> cont 106;
	PT _ (TS _ 52) -> cont 107;
	PT _ (TS _ 53) -> cont 108;
	PT _ (TS _ 54) -> cont 109;
	PT _ (TS _ 55) -> cont 110;
	PT _ (TS _ 56) -> cont 111;
	PT _ (TS _ 57) -> cont 112;
	PT _ (TS _ 58) -> cont 113;
	PT _ (TS _ 59) -> cont 114;
	PT _ (TS _ 60) -> cont 115;
	PT _ (TS _ 61) -> cont 116;
	PT _ (TV _) -> cont 117;
	PT _ (TI _) -> cont 118;
	PT _ (TD _) -> cont 119;
	PT _ (TC _) -> cont 120;
	PT _ (TL _) -> cont 121;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 122 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
    Bad $ "syntax error at " ++ tokenPos ts ++
    case ts of
        []      -> []
        [Err _] -> " due to lexer error"
        t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

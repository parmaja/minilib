unit snow2;
{*
 *  FAST IMPLEMENTATION OF STREAM CIPHER SNOW 2.0
 *
 *  Program: snowblock2.c
 *
 * Synopsis:
 *      Hard coded implementation of SNOW 2.0 stream cipher.
 *      Each call to SnowKeystreamBlock generates 16 words of
 *      keystream symbols.
 *
 * Ported to FPC from Snow2tab and Snow2Fast to Snow2.pas
 * Zaher Dirkey zaher@parmaja.com
 * With modification
 * Converted to class
 *
 *}

{$IFDEF FPC}
{$MODE delphi}
{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  u32 = Cardinal;
  u8 = Byte;

  TSnowKeySize = (key128, key256);
  TSnowKey = array[0..255] of byte;
  TSnowBlock = array[0..15] of u32;

  TSnowContext = record
    s: TSnowBlock;
    r1, r2: u32;
  end;

const
  cSnowBlockSize = SizeOf(TSnowBlock);

procedure SnowLoadkey(var Context: TSnowContext; Key: TSnowKey; KeySize: TSnowKeySize; IV3, IV2, IV1, IV0: u32);
procedure SnowKeyStreamBlock(var Context: TSnowContext; var Block: TSnowBlock);

function SnowGetByte(n: Byte; w: u32): Byte;

implementation

(*
 * TABLES FOR STREAM CIPHER SNOW 2.0
 *
 * Filename: snow2tab.h
 *
 * Authors:
 * Patrik Ekdahl & Thomas Johansson
 * Dept. of Information Technology
 * P.O. Box 118
 * SE-221 00 Lund, Sweden,
 * email: {patrik,thomas}@it.lth.se
 *
 * Synopsis:
 *   Defines multiplication with alpha and alpha^-1
 *   as well as the S-box
*)

 const
  TSnow_Alpha_Mul: array[0..255] of u32 = (
          $0, $E19FCF13, $6B973726, $8A08F835, $D6876E4C, $3718A15F, $BD10596A, $5C8F9679,
    $5A7DC98, $E438138B, $6E30EBBE, $8FAF24AD, $D320B2D4, $32BF7DC7, $B8B785F2, $59284AE1,
    $AE71199, $EB78DE8A, $617026BF, $80EFE9AC, $DC607FD5, $3DFFB0C6, $B7F748F3, $566887E0,
    $F40CD01, $EEDF0212, $64D7FA27, $85483534, $D9C7A34D, $38586C5E, $B250946B, $53CF5B78,
    $1467229B, $F5F8ED88, $7FF015BD, $9E6FDAAE, $C2E04CD7, $237F83C4, $A9777BF1, $48E8B4E2,
    $11C0FE03, $F05F3110, $7A57C925, $9BC80636, $C747904F, $26D85F5C, $ACD0A769, $4D4F687A,
    $1E803302, $FF1FFC11, $75170424, $9488CB37, $C8075D4E, $2998925D, $A3906A68, $420FA57B,
    $1B27EF9A, $FAB82089, $70B0D8BC, $912F17AF, $CDA081D6, $2C3F4EC5, $A637B6F0, $47A879E3,
    $28CE449F, $C9518B8C, $435973B9, $A2C6BCAA, $FE492AD3, $1FD6E5C0, $95DE1DF5, $7441D2E6,
    $2D699807, $CCF65714, $46FEAF21, $A7616032, $FBEEF64B, $1A713958, $9079C16D, $71E60E7E,
    $22295506, $C3B69A15, $49BE6220, $A821AD33, $F4AE3B4A, $1531F459, $9F390C6C, $7EA6C37F,
    $278E899E, $C611468D, $4C19BEB8, $AD8671AB, $F109E7D2, $109628C1, $9A9ED0F4, $7B011FE7,
    $3CA96604, $DD36A917, $573E5122, $B6A19E31, $EA2E0848, $BB1C75B, $81B93F6E, $6026F07D,
    $390EBA9C, $D891758F, $52998DBA, $B30642A9, $EF89D4D0, $E161BC3, $841EE3F6, $65812CE5,
    $364E779D, $D7D1B88E, $5DD940BB, $BC468FA8, $E0C919D1, $156D6C2, $8B5E2EF7, $6AC1E1E4,
    $33E9AB05, $D2766416, $587E9C23, $B9E15330, $E56EC549, $4F10A5A, $8EF9F26F, $6F663D7C,
    $50358897, $B1AA4784, $3BA2BFB1, $DA3D70A2, $86B2E6DB, $672D29C8, $ED25D1FD, $CBA1EEE,
    $5592540F, $B40D9B1C, $3E056329, $DF9AAC3A, $83153A43, $628AF550, $E8820D65, $91DC276,
    $5AD2990E, $BB4D561D, $3145AE28, $D0DA613B, $8C55F742, $6DCA3851, $E7C2C064, $65D0F77,
    $5F754596, $BEEA8A85, $34E272B0, $D57DBDA3, $89F22BDA, $686DE4C9, $E2651CFC, $3FAD3EF,
    $4452AA0C, $A5CD651F, $2FC59D2A, $CE5A5239, $92D5C440, $734A0B53, $F942F366, $18DD3C75,
    $41F57694, $A06AB987, $2A6241B2, $CBFD8EA1, $977218D8, $76EDD7CB, $FCE52FFE, $1D7AE0ED,
    $4EB5BB95, $AF2A7486, $25228CB3, $C4BD43A0, $9832D5D9, $79AD1ACA, $F3A5E2FF, $123A2DEC,
    $4B12670D, $AA8DA81E, $2085502B, $C11A9F38, $9D950941, $7C0AC652, $F6023E67, $179DF174,
    $78FBCC08, $9964031B, $136CFB2E, $F2F3343D, $AE7CA244, $4FE36D57, $C5EB9562, $24745A71,
    $7D5C1090, $9CC3DF83, $16CB27B6, $F754E8A5, $ABDB7EDC, $4A44B1CF, $C04C49FA, $21D386E9,
    $721CDD91, $93831282, $198BEAB7, $F81425A4, $A49BB3DD, $45047CCE, $CF0C84FB, $2E934BE8,
    $77BB0109, $9624CE1A, $1C2C362F, $FDB3F93C, $A13C6F45, $40A3A056, $CAAB5863, $2B349770,
    $6C9CEE93, $8D032180, $70BD9B5, $E69416A6, $BA1B80DF, $5B844FCC, $D18CB7F9, $301378EA,
    $693B320B, $88A4FD18, $2AC052D, $E333CA3E, $BFBC5C47, $5E239354, $D42B6B61, $35B4A472,
    $667BFF0A, $87E43019, $DECC82C, $EC73073F, $B0FC9146, $51635E55, $DB6BA660, $3AF46973,
    $63DC2392, $8243EC81, $84B14B4, $E9D4DBA7, $B55B4DDE, $54C482CD, $DECC7AF8, $3F53B5EB);


  TSnow_alphainv_mul: array[0..255] of u32 = (
           $0, $180F40CD, $301E8033, $2811C0FE, $603CA966, $7833E9AB, $50222955, $482D6998,
    $C078FBCC, $D877BB01, $F0667BFF, $E8693B32, $A04452AA, $B84B1267, $905AD299, $88559254,
    $29F05F31, $31FF1FFC, $19EEDF02, $1E19FCF, $49CCF657, $51C3B69A, $79D27664, $61DD36A9,
    $E988A4FD, $F187E430, $D99624CE, $C1996403, $89B40D9B, $91BB4D56, $B9AA8DA8, $A1A5CD65,
    $5249BE62, $4A46FEAF, $62573E51, $7A587E9C, $32751704, $2A7A57C9, $26B9737, $1A64D7FA,
    $923145AE, $8A3E0563, $A22FC59D, $BA208550, $F20DECC8, $EA02AC05, $C2136CFB, $DA1C2C36,
    $7BB9E153, $63B6A19E, $4BA76160, $53A821AD, $1B854835, $38A08F8, $2B9BC806, $339488CB,
    $BBC11A9F, $A3CE5A52, $8BDF9AAC, $93D0DA61, $DBFDB3F9, $C3F2F334, $EBE333CA, $F3EC7307,
    $A492D5C4, $BC9D9509, $948C55F7, $8C83153A, $C4AE7CA2, $DCA13C6F, $F4B0FC91, $ECBFBC5C,
    $64EA2E08, $7CE56EC5, $54F4AE3B, $4CFBEEF6, $4D6876E, $1CD9C7A3, $34C8075D, $2CC74790,
    $8D628AF5, $956DCA38, $BD7C0AC6, $A5734A0B, $ED5E2393, $F551635E, $DD40A3A0, $C54FE36D,
    $4D1A7139, $551531F4, $7D04F10A, $650BB1C7, $2D26D85F, $35299892, $1D38586C, $53718A1,
    $F6DB6BA6, $EED42B6B, $C6C5EB95, $DECAAB58, $96E7C2C0, $8EE8820D, $A6F942F3, $BEF6023E,
    $36A3906A, $2EACD0A7, $6BD1059, $1EB25094, $569F390C, $4E9079C1, $6681B93F, $7E8EF9F2,
    $DF2B3497, $C724745A, $EF35B4A4, $F73AF469, $BF179DF1, $A718DD3C, $8F091DC2, $97065D0F,
    $1F53CF5B, $75C8F96, $2F4D4F68, $37420FA5, $7F6F663D, $676026F0, $4F71E60E, $577EA6C3,
    $E18D0321, $F98243EC, $D1938312, $C99CC3DF, $81B1AA47, $99BEEA8A, $B1AF2A74, $A9A06AB9,
    $21F5F8ED, $39FAB820, $11EB78DE, $9E43813, $41C9518B, $59C61146, $71D7D1B8, $69D89175,
    $C87D5C10, $D0721CDD, $F863DC23, $E06C9CEE, $A841F576, $B04EB5BB, $985F7545, $80503588,
    $805A7DC, $100AE711, $381B27EF, $20146722, $68390EBA, $70364E77, $58278E89, $4028CE44,
    $B3C4BD43, $ABCBFD8E, $83DA3D70, $9BD57DBD, $D3F81425, $CBF754E8, $E3E69416, $FBE9D4DB,
    $73BC468F, $6BB30642, $43A2C6BC, $5BAD8671, $1380EFE9, $B8FAF24, $239E6FDA, $3B912F17,
    $9A34E272, $823BA2BF, $AA2A6241, $B225228C, $FA084B14, $E2070BD9, $CA16CB27, $D2198BEA,
    $5A4C19BE, $42435973, $6A52998D, $725DD940, $3A70B0D8, $227FF015, $A6E30EB, $12617026,
    $451FD6E5, $5D109628, $750156D6, $6D0E161B, $25237F83, $3D2C3F4E, $153DFFB0, $D32BF7D,
    $85672D29, $9D686DE4, $B579AD1A, $AD76EDD7, $E55B844F, $FD54C482, $D545047C, $CD4A44B1,
    $6CEF89D4, $74E0C919, $5CF109E7, $44FE492A, $CD320B2, $14DC607F, $3CCDA081, $24C2E04C,
    $AC977218, $B49832D5, $9C89F22B, $8486B2E6, $CCABDB7E, $D4A49BB3, $FCB55B4D, $E4BA1B80,
    $17566887, $F59284A, $2748E8B4, $3F47A879, $776AC1E1, $6F65812C, $477441D2, $5F7B011F,
    $D72E934B, $CF21D386, $E7301378, $FF3F53B5, $B7123A2D, $AF1D7AE0, $870CBA1E, $9F03FAD3,
    $3EA637B6, $26A9777B, $EB8B785, $16B7F748, $5E9A9ED0, $4695DE1D, $6E841EE3, $768B5E2E,
    $FEDECC7A, $E6D18CB7, $CEC04C49, $D6CF0C84, $9EE2651C, $86ED25D1, $AEFCE52F, $B6F3A5E2);


  TSnow_T0: array[0..255] of u32 = (
    $A56363C6, $847C7CF8, $997777EE, $8D7B7BF6, $DF2F2FF, $BD6B6BD6, $B16F6FDE, $54C5C591,
    $50303060, $3010102, $A96767CE, $7D2B2B56, $19FEFEE7, $62D7D7B5, $E6ABAB4D, $9A7676EC,
    $45CACA8F, $9D82821F, $40C9C989, $877D7DFA, $15FAFAEF, $EB5959B2, $C947478E, $BF0F0FB,
    $ECADAD41, $67D4D4B3, $FDA2A25F, $EAAFAF45, $BF9C9C23, $F7A4A453, $967272E4, $5BC0C09B,
    $C2B7B775, $1CFDFDE1, $AE93933D, $6A26264C, $5A36366C, $413F3F7E, $2F7F7F5, $4FCCCC83,
    $5C343468, $F4A5A551, $34E5E5D1, $8F1F1F9, $937171E2, $73D8D8AB, $53313162, $3F15152A,
    $C040408, $52C7C795, $65232346, $5EC3C39D, $28181830, $A1969637, $F05050A, $B59A9A2F,
    $907070E, $36121224, $9B80801B, $3DE2E2DF, $26EBEBCD, $6927274E, $CDB2B27F, $9F7575EA,
    $1B090912, $9E83831D, $742C2C58, $2E1A1A34, $2D1B1B36, $B26E6EDC, $EE5A5AB4, $FBA0A05B,
    $F65252A4, $4D3B3B76, $61D6D6B7, $CEB3B37D, $7B292952, $3EE3E3DD, $712F2F5E, $97848413,
    $F55353A6, $68D1D1B9, $0, $2CEDEDC1, $60202040, $1FFCFCE3, $C8B1B179, $ED5B5BB6,
    $BE6A6AD4, $46CBCB8D, $D9BEBE67, $4B393972, $DE4A4A94, $D44C4C98, $E85858B0, $4ACFCF85,
    $6BD0D0BB, $2AEFEFC5, $E5AAAA4F, $16FBFBED, $C5434386, $D74D4D9A, $55333366, $94858511,
    $CF45458A, $10F9F9E9, $6020204, $817F7FFE, $F05050A0, $443C3C78, $BA9F9F25, $E3A8A84B,
    $F35151A2, $FEA3A35D, $C0404080, $8A8F8F05, $AD92923F, $BC9D9D21, $48383870, $4F5F5F1,
    $DFBCBC63, $C1B6B677, $75DADAAF, $63212142, $30101020, $1AFFFFE5, $EF3F3FD, $6DD2D2BF,
    $4CCDCD81, $140C0C18, $35131326, $2FECECC3, $E15F5FBE, $A2979735, $CC444488, $3917172E,
    $57C4C493, $F2A7A755, $827E7EFC, $473D3D7A, $AC6464C8, $E75D5DBA, $2B191932, $957373E6,
    $A06060C0, $98818119, $D14F4F9E, $7FDCDCA3, $66222244, $7E2A2A54, $AB90903B, $8388880B,
    $CA46468C, $29EEEEC7, $D3B8B86B, $3C141428, $79DEDEA7, $E25E5EBC, $1D0B0B16, $76DBDBAD,
    $3BE0E0DB, $56323264, $4E3A3A74, $1E0A0A14, $DB494992, $A06060C, $6C242448, $E45C5CB8,
    $5DC2C29F, $6ED3D3BD, $EFACAC43, $A66262C4, $A8919139, $A4959531, $37E4E4D3, $8B7979F2,
    $32E7E7D5, $43C8C88B, $5937376E, $B76D6DDA, $8C8D8D01, $64D5D5B1, $D24E4E9C, $E0A9A949,
    $B46C6CD8, $FA5656AC, $7F4F4F3, $25EAEACF, $AF6565CA, $8E7A7AF4, $E9AEAE47, $18080810,
    $D5BABA6F, $887878F0, $6F25254A, $722E2E5C, $241C1C38, $F1A6A657, $C7B4B473, $51C6C697,
    $23E8E8CB, $7CDDDDA1, $9C7474E8, $211F1F3E, $DD4B4B96, $DCBDBD61, $868B8B0D, $858A8A0F,
    $907070E0, $423E3E7C, $C4B5B571, $AA6666CC, $D8484890, $5030306, $1F6F6F7, $120E0E1C,
    $A36161C2, $5F35356A, $F95757AE, $D0B9B969, $91868617, $58C1C199, $271D1D3A, $B99E9E27,
    $38E1E1D9, $13F8F8EB, $B398982B, $33111122, $BB6969D2, $70D9D9A9, $898E8E07, $A7949433,
    $B69B9B2D, $221E1E3C, $92878715, $20E9E9C9, $49CECE87, $FF5555AA, $78282850, $7ADFDFA5,
    $8F8C8C03, $F8A1A159, $80898909, $170D0D1A, $DABFBF65, $31E6E6D7, $C6424284, $B86868D0,
    $C3414182, $B0999929, $772D2D5A, $110F0F1E, $CBB0B07B, $FC5454A8, $D6BBBB6D, $3A16162C);

  TSnow_T1: array[0..255] of u32 = (
    $6363C6A5, $7C7CF884, $7777EE99, $7B7BF68D, $F2F2FF0D, $6B6BD6BD, $6F6FDEB1, $C5C59154,
    $30306050, $1010203, $6767CEA9, $2B2B567D, $FEFEE719, $D7D7B562, $ABAB4DE6, $7676EC9A,
    $CACA8F45, $82821F9D, $C9C98940, $7D7DFA87, $FAFAEF15, $5959B2EB, $47478EC9, $F0F0FB0B,
    $ADAD41EC, $D4D4B367, $A2A25FFD, $AFAF45EA, $9C9C23BF, $A4A453F7, $7272E496, $C0C09B5B,
    $B7B775C2, $FDFDE11C, $93933DAE, $26264C6A, $36366C5A, $3F3F7E41, $F7F7F502, $CCCC834F,
    $3434685C, $A5A551F4, $E5E5D134, $F1F1F908, $7171E293, $D8D8AB73, $31316253, $15152A3F,
    $404080C, $C7C79552, $23234665, $C3C39D5E, $18183028, $969637A1, $5050A0F, $9A9A2FB5,
    $7070E09, $12122436, $80801B9B, $E2E2DF3D, $EBEBCD26, $27274E69, $B2B27FCD, $7575EA9F,
    $909121B, $83831D9E, $2C2C5874, $1A1A342E, $1B1B362D, $6E6EDCB2, $5A5AB4EE, $A0A05BFB,
    $5252A4F6, $3B3B764D, $D6D6B761, $B3B37DCE, $2929527B, $E3E3DD3E, $2F2F5E71, $84841397,
    $5353A6F5, $D1D1B968, $0, $EDEDC12C, $20204060, $FCFCE31F, $B1B179C8, $5B5BB6ED,
    $6A6AD4BE, $CBCB8D46, $BEBE67D9, $3939724B, $4A4A94DE, $4C4C98D4, $5858B0E8, $CFCF854A,
    $D0D0BB6B, $EFEFC52A, $AAAA4FE5, $FBFBED16, $434386C5, $4D4D9AD7, $33336655, $85851194,
    $45458ACF, $F9F9E910, $2020406, $7F7FFE81, $5050A0F0, $3C3C7844, $9F9F25BA, $A8A84BE3,
    $5151A2F3, $A3A35DFE, $404080C0, $8F8F058A, $92923FAD, $9D9D21BC, $38387048, $F5F5F104,
    $BCBC63DF, $B6B677C1, $DADAAF75, $21214263, $10102030, $FFFFE51A, $F3F3FD0E, $D2D2BF6D,
    $CDCD814C, $C0C1814, $13132635, $ECECC32F, $5F5FBEE1, $979735A2, $444488CC, $17172E39,
    $C4C49357, $A7A755F2, $7E7EFC82, $3D3D7A47, $6464C8AC, $5D5DBAE7, $1919322B, $7373E695,
    $6060C0A0, $81811998, $4F4F9ED1, $DCDCA37F, $22224466, $2A2A547E, $90903BAB, $88880B83,
    $46468CCA, $EEEEC729, $B8B86BD3, $1414283C, $DEDEA779, $5E5EBCE2, $B0B161D, $DBDBAD76,
    $E0E0DB3B, $32326456, $3A3A744E, $A0A141E, $494992DB, $6060C0A, $2424486C, $5C5CB8E4,
    $C2C29F5D, $D3D3BD6E, $ACAC43EF, $6262C4A6, $919139A8, $959531A4, $E4E4D337, $7979F28B,
    $E7E7D532, $C8C88B43, $37376E59, $6D6DDAB7, $8D8D018C, $D5D5B164, $4E4E9CD2, $A9A949E0,
    $6C6CD8B4, $5656ACFA, $F4F4F307, $EAEACF25, $6565CAAF, $7A7AF48E, $AEAE47E9, $8081018,
    $BABA6FD5, $7878F088, $25254A6F, $2E2E5C72, $1C1C3824, $A6A657F1, $B4B473C7, $C6C69751,
    $E8E8CB23, $DDDDA17C, $7474E89C, $1F1F3E21, $4B4B96DD, $BDBD61DC, $8B8B0D86, $8A8A0F85,
    $7070E090, $3E3E7C42, $B5B571C4, $6666CCAA, $484890D8, $3030605, $F6F6F701, $E0E1C12,
    $6161C2A3, $35356A5F, $5757AEF9, $B9B969D0, $86861791, $C1C19958, $1D1D3A27, $9E9E27B9,
    $E1E1D938, $F8F8EB13, $98982BB3, $11112233, $6969D2BB, $D9D9A970, $8E8E0789, $949433A7,
    $9B9B2DB6, $1E1E3C22, $87871592, $E9E9C920, $CECE8749, $5555AAFF, $28285078, $DFDFA57A,
    $8C8C038F, $A1A159F8, $89890980, $D0D1A17, $BFBF65DA, $E6E6D731, $424284C6, $6868D0B8,
    $414182C3, $999929B0, $2D2D5A77, $F0F1E11, $B0B07BCB, $5454A8FC, $BBBB6DD6, $16162C3A);

  TSnow_T2: array[0..255] of u32 = (
    $63C6A563, $7CF8847C, $77EE9977, $7BF68D7B, $F2FF0DF2, $6BD6BD6B, $6FDEB16F, $C59154C5,
    $30605030, $1020301, $67CEA967, $2B567D2B, $FEE719FE, $D7B562D7, $AB4DE6AB, $76EC9A76,
    $CA8F45CA, $821F9D82, $C98940C9, $7DFA877D, $FAEF15FA, $59B2EB59, $478EC947, $F0FB0BF0,
    $AD41ECAD, $D4B367D4, $A25FFDA2, $AF45EAAF, $9C23BF9C, $A453F7A4, $72E49672, $C09B5BC0,
    $B775C2B7, $FDE11CFD, $933DAE93, $264C6A26, $366C5A36, $3F7E413F, $F7F502F7, $CC834FCC,
    $34685C34, $A551F4A5, $E5D134E5, $F1F908F1, $71E29371, $D8AB73D8, $31625331, $152A3F15,
    $4080C04, $C79552C7, $23466523, $C39D5EC3, $18302818, $9637A196, $50A0F05, $9A2FB59A,
    $70E0907, $12243612, $801B9B80, $E2DF3DE2, $EBCD26EB, $274E6927, $B27FCDB2, $75EA9F75,
    $9121B09, $831D9E83, $2C58742C, $1A342E1A, $1B362D1B, $6EDCB26E, $5AB4EE5A, $A05BFBA0,
    $52A4F652, $3B764D3B, $D6B761D6, $B37DCEB3, $29527B29, $E3DD3EE3, $2F5E712F, $84139784,
    $53A6F553, $D1B968D1, $0, $EDC12CED, $20406020, $FCE31FFC, $B179C8B1, $5BB6ED5B,
    $6AD4BE6A, $CB8D46CB, $BE67D9BE, $39724B39, $4A94DE4A, $4C98D44C, $58B0E858, $CF854ACF,
    $D0BB6BD0, $EFC52AEF, $AA4FE5AA, $FBED16FB, $4386C543, $4D9AD74D, $33665533, $85119485,
    $458ACF45, $F9E910F9, $2040602, $7FFE817F, $50A0F050, $3C78443C, $9F25BA9F, $A84BE3A8,
    $51A2F351, $A35DFEA3, $4080C040, $8F058A8F, $923FAD92, $9D21BC9D, $38704838, $F5F104F5,
    $BC63DFBC, $B677C1B6, $DAAF75DA, $21426321, $10203010, $FFE51AFF, $F3FD0EF3, $D2BF6DD2,
    $CD814CCD, $C18140C, $13263513, $ECC32FEC, $5FBEE15F, $9735A297, $4488CC44, $172E3917,
    $C49357C4, $A755F2A7, $7EFC827E, $3D7A473D, $64C8AC64, $5DBAE75D, $19322B19, $73E69573,
    $60C0A060, $81199881, $4F9ED14F, $DCA37FDC, $22446622, $2A547E2A, $903BAB90, $880B8388,
    $468CCA46, $EEC729EE, $B86BD3B8, $14283C14, $DEA779DE, $5EBCE25E, $B161D0B, $DBAD76DB,
    $E0DB3BE0, $32645632, $3A744E3A, $A141E0A, $4992DB49, $60C0A06, $24486C24, $5CB8E45C,
    $C29F5DC2, $D3BD6ED3, $AC43EFAC, $62C4A662, $9139A891, $9531A495, $E4D337E4, $79F28B79,
    $E7D532E7, $C88B43C8, $376E5937, $6DDAB76D, $8D018C8D, $D5B164D5, $4E9CD24E, $A949E0A9,
    $6CD8B46C, $56ACFA56, $F4F307F4, $EACF25EA, $65CAAF65, $7AF48E7A, $AE47E9AE, $8101808,
    $BA6FD5BA, $78F08878, $254A6F25, $2E5C722E, $1C38241C, $A657F1A6, $B473C7B4, $C69751C6,
    $E8CB23E8, $DDA17CDD, $74E89C74, $1F3E211F, $4B96DD4B, $BD61DCBD, $8B0D868B, $8A0F858A,
    $70E09070, $3E7C423E, $B571C4B5, $66CCAA66, $4890D848, $3060503, $F6F701F6, $E1C120E,
    $61C2A361, $356A5F35, $57AEF957, $B969D0B9, $86179186, $C19958C1, $1D3A271D, $9E27B99E,
    $E1D938E1, $F8EB13F8, $982BB398, $11223311, $69D2BB69, $D9A970D9, $8E07898E, $9433A794,
    $9B2DB69B, $1E3C221E, $87159287, $E9C920E9, $CE8749CE, $55AAFF55, $28507828, $DFA57ADF,
    $8C038F8C, $A159F8A1, $89098089, $D1A170D, $BF65DABF, $E6D731E6, $4284C642, $68D0B868,
    $4182C341, $9929B099, $2D5A772D, $F1E110F, $B07BCBB0, $54A8FC54, $BB6DD6BB, $162C3A16);

  TSnow_T3: array[0..255] of u32 = (
    $C6A56363, $F8847C7C, $EE997777, $F68D7B7B, $FF0DF2F2, $D6BD6B6B, $DEB16F6F, $9154C5C5,
    $60503030, $2030101, $CEA96767, $567D2B2B, $E719FEFE, $B562D7D7, $4DE6ABAB, $EC9A7676,
    $8F45CACA, $1F9D8282, $8940C9C9, $FA877D7D, $EF15FAFA, $B2EB5959, $8EC94747, $FB0BF0F0,
    $41ECADAD, $B367D4D4, $5FFDA2A2, $45EAAFAF, $23BF9C9C, $53F7A4A4, $E4967272, $9B5BC0C0,
    $75C2B7B7, $E11CFDFD, $3DAE9393, $4C6A2626, $6C5A3636, $7E413F3F, $F502F7F7, $834FCCCC,
    $685C3434, $51F4A5A5, $D134E5E5, $F908F1F1, $E2937171, $AB73D8D8, $62533131, $2A3F1515,
    $80C0404, $9552C7C7, $46652323, $9D5EC3C3, $30281818, $37A19696, $A0F0505, $2FB59A9A,
    $E090707, $24361212, $1B9B8080, $DF3DE2E2, $CD26EBEB, $4E692727, $7FCDB2B2, $EA9F7575,
    $121B0909, $1D9E8383, $58742C2C, $342E1A1A, $362D1B1B, $DCB26E6E, $B4EE5A5A, $5BFBA0A0,
    $A4F65252, $764D3B3B, $B761D6D6, $7DCEB3B3, $527B2929, $DD3EE3E3, $5E712F2F, $13978484,
    $A6F55353, $B968D1D1, $0, $C12CEDED, $40602020, $E31FFCFC, $79C8B1B1, $B6ED5B5B,
    $D4BE6A6A, $8D46CBCB, $67D9BEBE, $724B3939, $94DE4A4A, $98D44C4C, $B0E85858, $854ACFCF,
    $BB6BD0D0, $C52AEFEF, $4FE5AAAA, $ED16FBFB, $86C54343, $9AD74D4D, $66553333, $11948585,
    $8ACF4545, $E910F9F9, $4060202, $FE817F7F, $A0F05050, $78443C3C, $25BA9F9F, $4BE3A8A8,
    $A2F35151, $5DFEA3A3, $80C04040, $58A8F8F, $3FAD9292, $21BC9D9D, $70483838, $F104F5F5,
    $63DFBCBC, $77C1B6B6, $AF75DADA, $42632121, $20301010, $E51AFFFF, $FD0EF3F3, $BF6DD2D2,
    $814CCDCD, $18140C0C, $26351313, $C32FECEC, $BEE15F5F, $35A29797, $88CC4444, $2E391717,
    $9357C4C4, $55F2A7A7, $FC827E7E, $7A473D3D, $C8AC6464, $BAE75D5D, $322B1919, $E6957373,
    $C0A06060, $19988181, $9ED14F4F, $A37FDCDC, $44662222, $547E2A2A, $3BAB9090, $B838888,
    $8CCA4646, $C729EEEE, $6BD3B8B8, $283C1414, $A779DEDE, $BCE25E5E, $161D0B0B, $AD76DBDB,
    $DB3BE0E0, $64563232, $744E3A3A, $141E0A0A, $92DB4949, $C0A0606, $486C2424, $B8E45C5C,
    $9F5DC2C2, $BD6ED3D3, $43EFACAC, $C4A66262, $39A89191, $31A49595, $D337E4E4, $F28B7979,
    $D532E7E7, $8B43C8C8, $6E593737, $DAB76D6D, $18C8D8D, $B164D5D5, $9CD24E4E, $49E0A9A9,
    $D8B46C6C, $ACFA5656, $F307F4F4, $CF25EAEA, $CAAF6565, $F48E7A7A, $47E9AEAE, $10180808,
    $6FD5BABA, $F0887878, $4A6F2525, $5C722E2E, $38241C1C, $57F1A6A6, $73C7B4B4, $9751C6C6,
    $CB23E8E8, $A17CDDDD, $E89C7474, $3E211F1F, $96DD4B4B, $61DCBDBD, $D868B8B, $F858A8A,
    $E0907070, $7C423E3E, $71C4B5B5, $CCAA6666, $90D84848, $6050303, $F701F6F6, $1C120E0E,
    $C2A36161, $6A5F3535, $AEF95757, $69D0B9B9, $17918686, $9958C1C1, $3A271D1D, $27B99E9E,
    $D938E1E1, $EB13F8F8, $2BB39898, $22331111, $D2BB6969, $A970D9D9, $7898E8E, $33A79494,
    $2DB69B9B, $3C221E1E, $15928787, $C920E9E9, $8749CECE, $AAFF5555, $50782828, $A57ADFDF,
    $38F8C8C, $59F8A1A1, $9808989, $1A170D0D, $65DABFBF, $D731E6E6, $84C64242, $D0B86868,
    $82C34141, $29B09999, $5A772D2D, $1E110F0F, $7BCBB0B0, $A8FC5454, $6DD6BBBB, $2C3A1616);

function SnowGetByte(n: Byte; w: u32): Byte;
begin
  Result := (((w) shr (n * 8)) and $FF);
end;

function ainv_mul(w: u32): u32;
begin
  Result := (((w) shr 8) xor (TSnow_Alphainv_Mul[w and $FF]));
end;

function a_mul(w: u32): u32;
begin
  Result := (((w) shl 8) xor (TSnow_alpha_mul[w shr 24]));
end;

{*
 * Function:  snow_loadkey_fast
 *
 * Synopsis:
 *   Loads the Key material and performs the initial mixing.
 *
 * Assumptions:
 *   keysize is either 128 or 256.
 *   Key is of proper length, for keysize=128, Key is of lenght 16 bytes
 *      and for keysize=256, Key is of length 32 bytes.
 *   Key is given in big endian format,
 *   For 128 bit Key:
 *        Key[0]-> msb of k_3
 *         ...
 *        Key[3]-> lsb of k_3
 *         ...
 *        Key[12]-> msb of k_0
 *         ...
 *        Key[15]-> lsb of k_0
 *
 *   For 256 bit Key:
 *        Key[0]-> msb of k_7
 *          ...
 *        Key[3]-> lsb of k_7
 *          ...
 *        Key[28]-> msb of k_0
 *          ...
 *        Key[31]-> lsb of k_0
 *}

procedure SnowLoadkey(var Context: TSnowContext; Key: TSnowKey; KeySize: TSnowKeySize; IV3, IV2, IV1, IV0: u32);
var
  i: Integer;
  fsmtmp, outfrom_fsm: u32;
begin
  with Context do
  begin
    if (KeySize = key128) then
    begin
      s[15] := (Key[0] shl 24)  or (Key[1] shl 16) or
             (Key[2] shl 8)   or Key[3];
      s[14] := (Key[4] shl 24)  or (Key[5] shl 16) or
             (Key[6] shl 8)   or Key[7];
      s[13] := (Key[8] shl 24)  or (Key[9] shl 16) or
             (Key[10] shl 8)  or Key[11];
      s[12] := (Key[12] shl 24) or (Key[13] shl 16) or
             (Key[14] shl 8)  or Key[15];
      s[11] := not s[15]; { bitwise inverse }
      s[10] := not s[14];
      s[9]:= not s[13];
      s[8]:= not s[12];
      s[7]:= s[15]; { just copy }
      s[6]:= s[14];
      s[5]:= s[13];
      s[4]:= s[12];
      s[3]:= not s[15]; { bitwise inverse }
      s[2]:= not s[14];
      s[1]:= not s[13];
      s[0]:= not s[12];
    end
    else
    begin { assume keysize=256 }
      s[15] := (Key[0] shl 24) or (Key[1] shl 16) or
             (Key[2] shl 8) or Key[3];
      s[14] := (Key[4] shl 24) or (Key[5] shl 16) or
             (Key[6] shl 8) or Key[7];
      s[13] := (Key[8] shl 24) or (Key[9] shl 16) or
             (Key[10] shl 8) or Key[11];
      s[12] := (Key[12] shl 24) or (Key[13] shl 16) or
             (Key[14] shl 8) or Key[15];

      s[11] := (Key[16] shl 24) or (Key[17] shl 16) or
             (Key[18] shl 8) or Key[19];
      s[10] := (Key[20] shl 24) or (Key[21] shl 16) or
             (Key[22] shl 8) or Key[23];
      s[9] := (Key[24] shl 24) or (Key[25] shl 16) or
             (Key[26] shl 8) or Key[27];
      s[8] := (Key[28] shl 24) or (Key[29] shl 16) or
             (Key[30] shl 8) or Key[31];

      s[7]:= not s[15]; { bitwise inverse }
      s[6]:= not s[14];
      s[5]:= not s[13];
      s[4]:= not s[12];
      s[3]:= not s[11];
      s[2]:= not s[10];
      s[1]:= not s[9];
      s[0]:= not s[8];
    end;

    { XOR IV values }
    s[15] := s[15] xor IV0;
    s[12] := s[12] xor IV1;
    s[10] := s[10] xor IV2;
    s[9]:= s[9]xor IV3;

    r1 := 0;
    r2 := 0;

    { Do 32 initial clockings }
    for i := 0 to 1 do
    begin
      outfrom_fsm := (r1 + s[15]) xor r2;
      s[0] := a_mul(s[0]) xor s[2] xor ainv_mul(s[11]) xor outfrom_fsm;
      fsmtmp := r2 + s[5];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[0]) xor r2;
      s[1] := a_mul(s[1]) xor s[3] xor ainv_mul(s[12]) xor outfrom_fsm;
      fsmtmp := r2 + s[6];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[1]) xor r2;
      s[2] := a_mul(s[2]) xor s[4] xor ainv_mul(s[13]) xor outfrom_fsm;
      fsmtmp := r2 + s[7];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[2]) xor r2;
      s[3] := a_mul(s[3]) xor s[5] xor ainv_mul(s[14]) xor outfrom_fsm;
      fsmtmp := r2 + s[8];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[3]) xor r2;
      s[4] := a_mul(s[4]) xor s[6] xor ainv_mul(s[15]) xor outfrom_fsm;
      fsmtmp := r2 + s[9];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[4]) xor r2;
      s[5] := a_mul(s[5]) xor s[7] xor ainv_mul(s[0]) xor outfrom_fsm;
      fsmtmp := r2 + s[10];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[5]) xor r2;
      s[6] := a_mul(s[6]) xor s[8] xor ainv_mul(s[1]) xor outfrom_fsm;
      fsmtmp := r2 + s[11];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[6]) xor r2;
      s[7] := a_mul(s[7]) xor s[9] xor ainv_mul(s[2]) xor outfrom_fsm;
      fsmtmp := r2 + s[12];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[7]) xor r2;
      s[8] := a_mul(s[8]) xor s[10] xor ainv_mul(s[3]) xor outfrom_fsm;
      fsmtmp := r2 + s[13];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[8]) xor r2;
      s[9] := a_mul(s[9]) xor s[11] xor ainv_mul(s[4]) xor outfrom_fsm;
      fsmtmp := r2 + s[14];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[9]) xor r2;
      s[10] := a_mul(s[10]) xor s[12] xor ainv_mul(s[5]) xor outfrom_fsm;
      fsmtmp := r2 + s[15];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[10]) xor r2;
      s[11] := a_mul(s[11]) xor s[13] xor ainv_mul(s[6]) xor outfrom_fsm;
      fsmtmp := r2 + s[0];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[11]) xor r2;
      s[12] := a_mul(s[12]) xor s[14] xor ainv_mul(s[7]) xor outfrom_fsm;
      fsmtmp := r2 + s[1];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[12]) xor r2;
      s[13] := a_mul(s[13]) xor s[15] xor ainv_mul(s[8]) xor outfrom_fsm;
      fsmtmp := r2 + s[2];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[13]) xor r2;
      s[14] := a_mul(s[14]) xor s[0] xor ainv_mul(s[9]) xor outfrom_fsm;
      fsmtmp := r2 + s[3];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;

      outfrom_fsm := (r1 + s[14]) xor r2;
      s[15] := a_mul(s[15]) xor s[1] xor ainv_mul(s[10]) xor outfrom_fsm;
      fsmtmp := r2 + s[4];
      r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
      r1 := fsmtmp;
    end;
  end;
end;

procedure SnowKeyStreamBlock(var Context: TSnowContext; var Block: TSnowBlock);
var
  fsmtmp: u32;
begin
  with Context do
  begin
    s[0] := a_mul(s[0]) xor s[2] xor ainv_mul(s[11]);
    fsmtmp := r2 + s[5];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[0] := (r1 + s[0]) xor r2 xor s[1];

    s[1] := a_mul(s[1]) xor s[3] xor ainv_mul(s[12]);
    fsmtmp := r2 + s[6];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[1] := (r1 + s[1]) xor r2 xor s[2];

    s[2] := a_mul(s[2]) xor s[4] xor ainv_mul(s[13]);
    fsmtmp := r2 + s[7];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[2] := (r1 + s[2]) xor r2 xor s[3];

    s[3] := a_mul(s[3]) xor s[5] xor ainv_mul(s[14]);
    fsmtmp := r2 + s[8];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[3] := (r1 + s[3]) xor r2 xor s[4];

    s[4] := a_mul(s[4]) xor s[6] xor ainv_mul(s[15]);
    fsmtmp := r2 + s[9];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[4] := (r1 + s[4]) xor r2 xor s[5];

    s[5] := a_mul(s[5]) xor s[7] xor ainv_mul(s[0]);
    fsmtmp := r2 + s[10];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[5] := (r1 + s[5]) xor r2 xor s[6];

    s[6] := a_mul(s[6]) xor s[8] xor ainv_mul(s[1]);
    fsmtmp := r2 + s[11];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[6] := (r1 + s[6]) xor r2 xor s[7];

    s[7] := a_mul(s[7]) xor s[9] xor ainv_mul(s[2]);
    fsmtmp := r2 + s[12];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[7] := (r1 + s[7]) xor r2 xor s[8];

    s[8] := a_mul(s[8]) xor s[10] xor ainv_mul(s[3]);
    fsmtmp := r2 + s[13];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[8] := (r1 + s[8]) xor r2 xor s[9];

    s[9] := a_mul(s[9]) xor s[11] xor ainv_mul(s[4]);
    fsmtmp := r2 + s[14];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[9] := (r1 + s[9]) xor r2 xor s[10];

    s[10] := a_mul(s[10]) xor s[12] xor ainv_mul(s[5]);
    fsmtmp := r2 + s[15];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[10] := (r1 + s[10]) xor r2 xor s[11];

    s[11] := a_mul(s[11]) xor s[13] xor ainv_mul(s[6]);
    fsmtmp := r2 + s[0];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[11] := (r1 + s[11]) xor r2 xor s[12];

    s[12] := a_mul(s[12]) xor s[14] xor ainv_mul(s[7]);
    fsmtmp := r2 + s[1];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[12] := (r1 + s[12]) xor r2 xor s[13];

    s[13] := a_mul(s[13]) xor s[15] xor ainv_mul(s[8]);
    fsmtmp := r2 + s[2];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[13] := (r1 + s[13]) xor r2 xor s[14];

    s[14] := a_mul(s[14]) xor s[0] xor ainv_mul(s[9]);
    fsmtmp := r2 + s[3];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[14] := (r1 + s[14]) xor r2 xor s[15];

    s[15] := a_mul(s[15]) xor s[1] xor ainv_mul(s[10]);
    fsmtmp := r2 + s[4];
    r2 := TSnow_T0[SnowGetByte(0, r1)] xor TSnow_T1[SnowGetByte(1, r1)] xor TSnow_T2[SnowGetByte(2, r1)] xor TSnow_T3[SnowGetByte(3, r1)];
    r1 := fsmtmp;
    Block[15] := (r1 + s[15]) xor r2 xor s[0];
  end;
end;

end.


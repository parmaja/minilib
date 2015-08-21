unit BarcodeUtils;
{**
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    From Internet, I forget from where we got it
 * Modifying by:
 * @author    Belal alHamad 
 * @author    Zaher Dirkey
 *}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Contnrs, DrawUtils, TextUtils, Math;

const
  bcMaxBarCodeLen = 255;
  bcDefNarrowToWideRatio = 2;
type
  EStBarCodeError = class(Exception)
  end;

  { PDF417 types and constants }

  TStDataMode = (dmBinary, dmText, dmNumeric);

  //TStPDF417CodewordList = array [0..2700] of Word;
  TStPDF417CodewordList = array of Word;
  TStPDF417ECCLevels = (ecAuto, ecLevel0, ecLevel1, ecLevel2, ecLevel3,
    ecLevel4, ecLevel5, ecLevel6, ecLevel7, ecLevel8);

  TStPDF417TextCompactionMode = (cmAlpha, cmLower, cmMixed, cmPunctuation, cmNone);
  TStPDF417TextCompactionModes = set of TStPDF417TextCompactionMode;

  TStPDF417TextCompactionData = record
    Value: Integer;
    Mode: TStPDF417TextCompactionModes;
  end;

type
  TStPDF417CodewordArray = array[0..2] of array[0..928] of Longint;

const

  StPDF417CellWidth = 17;

  StPDF417Codewords: TstPDF417CodewordArray =
  (($1D5C0, $1EAF0, $1F57C, $1D4E0, $1EA78, $1F53E, $1A8C0, $1D470, $1A860,
    $15040, $1A830, $15020, $1ADC0, $1D6F0, $1EB7C, $1ACE0, $1D678, $1EB3E,
    $158C0, $1AC70, $15860, $15DC0, $1AEF0, $1D77C, $15CE0, $1AE78, $1D73E,
    $15C70, $1AE3C, $15EF0, $1AF7C, $15E78, $1AF3E, $15F7C, $1F5FA, $1D2E0,
    $1E978, $1F4BE, $1A4C0, $1D270, $1E93C, $1A460, $1D238, $14840, $1A430,
    $1D21C, $14820, $1A418, $14810, $1A6E0, $1D378, $1E9BE, $14CC0, $1A670,
    $1D33C, $14C60, $1A638, $1D31E, $14C30, $1A61C, $14EE0, $1A778, $1D3BE,
    $14E70, $1A73C, $14E38, $1A71E, $14F78, $1A7BE, $14F3C, $14F1E, $1A2C0,
    $1D170, $1E8BC, $1A260, $1D138, $1E89E, $14440, $1A230, $1D11C, $14420,
    $1A218, $14410, $14408, $146C0, $1A370, $1D1BC, $14660, $1A338, $1D19E,
    $14630, $1A31C, $14618, $1460C, $14770, $1A3BC, $14738, $1A39E, $1471C,
    $147BC, $1A160, $1D0B8, $1E85E, $14240, $1A130, $1D09C, $14220, $1A118,
    $1D08E, $14210, $1A10C, $14208, $1A106, $14360, $1A1B8, $1D0DE, $14330,
    $1A19C, $14318, $1A18E, $1430C, $14306, $1A1DE, $1438E, $14140, $1A0B0,
    $1D05C, $14120, $1A098, $1D04E, $14110, $1A08C, $14108, $1A086, $14104,
    $141B0, $14198, $1418C, $140A0, $1D02E, $1A04C, $1A046, $14082, $1CAE0,
    $1E578, $1F2BE, $194C0, $1CA70, $1E53C, $19460, $1CA38, $1E51E, $12840,
    $19430, $12820, $196E0, $1CB78, $1E5BE, $12CC0, $19670, $1CB3C, $12C60,
    $19638, $12C30, $12C18, $12EE0, $19778, $1CBBE, $12E70, $1973C, $12E38,
    $12E1C, $12F78, $197BE, $12F3C, $12FBE, $1DAC0, $1ED70, $1F6BC, $1DA60,
    $1ED38, $1F69E, $1B440, $1DA30, $1ED1C, $1B420, $1DA18, $1ED0E, $1B410,
    $1DA0C, $192C0, $1C970, $1E4BC, $1B6C0, $19260, $1C938, $1E49E, $1B660,
    $1DB38, $1ED9E, $16C40, $12420, $19218, $1C90E, $16C20, $1B618, $16C10,
    $126C0, $19370, $1C9BC, $16EC0, $12660, $19338, $1C99E, $16E60, $1B738,
    $1DB9E, $16E30, $12618, $16E18, $12770, $193BC, $16F70, $12738, $1939E,
    $16F38, $1B79E, $16F1C, $127BC, $16FBC, $1279E, $16F9E, $1D960, $1ECB8,
    $1F65E, $1B240, $1D930, $1EC9C, $1B220, $1D918, $1EC8E, $1B210, $1D90C,
    $1B208, $1B204, $19160, $1C8B8, $1E45E, $1B360, $19130, $1C89C, $16640,
    $12220, $1D99C, $1C88E, $16620, $12210, $1910C, $16610, $1B30C, $19106,
    $12204, $12360, $191B8, $1C8DE, $16760, $12330, $1919C, $16730, $1B39C,
    $1918E, $16718, $1230C, $12306, $123B8, $191DE, $167B8, $1239C, $1679C,
    $1238E, $1678E, $167DE, $1B140, $1D8B0, $1EC5C, $1B120, $1D898, $1EC4E,
    $1B110, $1D88C, $1B108, $1D886, $1B104, $1B102, $12140, $190B0, $1C85C,
    $16340, $12120, $19098, $1C84E, $16320, $1B198, $1D8CE, $16310, $12108,
    $19086, $16308, $1B186, $16304, $121B0, $190DC, $163B0, $12198, $190CE,
    $16398, $1B1CE, $1638C, $12186, $16386, $163DC, $163CE, $1B0A0, $1D858,
    $1EC2E, $1B090, $1D84C, $1B088, $1D846, $1B084, $1B082, $120A0, $19058,
    $1C82E, $161A0, $12090, $1904C, $16190, $1B0CC, $19046, $16188, $12084,
    $16184, $12082, $120D8, $161D8, $161CC, $161C6, $1D82C, $1D826, $1B042,
    $1902C, $12048, $160C8, $160C4, $160C2, $18AC0, $1C570, $1E2BC, $18A60,
    $1C538, $11440, $18A30, $1C51C, $11420, $18A18, $11410, $11408, $116C0,
    $18B70, $1C5BC, $11660, $18B38, $1C59E, $11630, $18B1C, $11618, $1160C,
    $11770, $18BBC, $11738, $18B9E, $1171C, $117BC, $1179E, $1CD60, $1E6B8,
    $1F35E, $19A40, $1CD30, $1E69C, $19A20, $1CD18, $1E68E, $19A10, $1CD0C,
    $19A08, $1CD06, $18960, $1C4B8, $1E25E, $19B60, $18930, $1C49C, $13640,
    $11220, $1CD9C, $1C48E, $13620, $19B18, $1890C, $13610, $11208, $13608,
    $11360, $189B8, $1C4DE, $13760, $11330, $1CDDE, $13730, $19B9C, $1898E,
    $13718, $1130C, $1370C, $113B8, $189DE, $137B8, $1139C, $1379C, $1138E,
    $113DE, $137DE, $1DD40, $1EEB0, $1F75C, $1DD20, $1EE98, $1F74E, $1DD10,
    $1EE8C, $1DD08, $1EE86, $1DD04, $19940, $1CCB0, $1E65C, $1BB40, $19920,
    $1EEDC, $1E64E, $1BB20, $1DD98, $1EECE, $1BB10, $19908, $1CC86, $1BB08,
    $1DD86, $19902, $11140, $188B0, $1C45C, $13340, $11120, $18898, $1C44E,
    $17740, $13320, $19998, $1CCCE, $17720, $1BB98, $1DDCE, $18886, $17710,
    $13308, $19986, $17708, $11102, $111B0, $188DC, $133B0, $11198, $188CE,
    $177B0, $13398, $199CE, $17798, $1BBCE, $11186, $13386, $111DC, $133DC,
    $111CE, $177DC, $133CE, $1DCA0, $1EE58, $1F72E, $1DC90, $1EE4C, $1DC88,
    $1EE46, $1DC84, $1DC82, $198A0, $1CC58, $1E62E, $1B9A0, $19890, $1EE6E,
    $1B990, $1DCCC, $1CC46, $1B988, $19884, $1B984, $19882, $1B982, $110A0,
    $18858, $1C42E, $131A0, $11090, $1884C, $173A0, $13190, $198CC, $18846,
    $17390, $1B9CC, $11084, $17388, $13184, $11082, $13182, $110D8, $1886E,
    $131D8, $110CC, $173D8, $131CC, $110C6, $173CC, $131C6, $110EE, $173EE,
    $1DC50, $1EE2C, $1DC48, $1EE26, $1DC44, $1DC42, $19850, $1CC2C, $1B8D0,
    $19848, $1CC26, $1B8C8, $1DC66, $1B8C4, $19842, $1B8C2, $11050, $1882C,
    $130D0, $11048, $18826, $171D0, $130C8, $19866, $171C8, $1B8E6, $11042,
    $171C4, $130C2, $171C2, $130EC, $171EC, $171E6, $1EE16, $1DC22, $1CC16,
    $19824, $19822, $11028, $13068, $170E8, $11022, $13062, $18560, $10A40,
    $18530, $10A20, $18518, $1C28E, $10A10, $1850C, $10A08, $18506, $10B60,
    $185B8, $1C2DE, $10B30, $1859C, $10B18, $1858E, $10B0C, $10B06, $10BB8,
    $185DE, $10B9C, $10B8E, $10BDE, $18D40, $1C6B0, $1E35C, $18D20, $1C698,
    $18D10, $1C68C, $18D08, $1C686, $18D04, $10940, $184B0, $1C25C, $11B40,
    $10920, $1C6DC, $1C24E, $11B20, $18D98, $1C6CE, $11B10, $10908, $18486,
    $11B08, $18D86, $10902, $109B0, $184DC, $11BB0, $10998, $184CE, $11B98,
    $18DCE, $11B8C, $10986, $109DC, $11BDC, $109CE, $11BCE, $1CEA0, $1E758,
    $1F3AE, $1CE90, $1E74C, $1CE88, $1E746, $1CE84, $1CE82, $18CA0, $1C658,
    $19DA0, $18C90, $1C64C, $19D90, $1CECC, $1C646, $19D88, $18C84, $19D84,
    $18C82, $19D82, $108A0, $18458, $119A0, $10890, $1C66E, $13BA0, $11990,
    $18CCC, $18446, $13B90, $19DCC, $10884, $13B88, $11984, $10882, $11982,
    $108D8, $1846E, $119D8, $108CC, $13BD8, $119CC, $108C6, $13BCC, $119C6,
    $108EE, $119EE, $13BEE, $1EF50, $1F7AC, $1EF48, $1F7A6, $1EF44, $1EF42,
    $1CE50, $1E72C, $1DED0, $1EF6C, $1E726, $1DEC8, $1EF66, $1DEC4, $1CE42,
    $1DEC2, $18C50, $1C62C, $19CD0, $18C48, $1C626, $1BDD0, $19CC8, $1CE66,
    $1BDC8, $1DEE6, $18C42, $1BDC4, $19CC2, $1BDC2, $10850, $1842C, $118D0,
    $10848, $18426, $139D0, $118C8, $18C66, $17BD0, $139C8, $19CE6, $10842,
    $17BC8, $1BDE6, $118C2, $17BC4, $1086C, $118EC, $10866, $139EC, $118E6,
    $17BEC, $139E6, $17BE6, $1EF28, $1F796, $1EF24, $1EF22, $1CE28, $1E716,
    $1DE68, $1EF36, $1DE64, $1CE22, $1DE62, $18C28, $1C616, $19C68, $18C24,
    $1BCE8, $19C64, $18C22, $1BCE4, $19C62, $1BCE2, $10828, $18416, $11868,
    $18C36, $138E8, $11864, $10822, $179E8, $138E4, $11862, $179E4, $138E2,
    $179E2, $11876, $179F6, $1EF12, $1DE34, $1DE32, $19C34, $1BC74, $1BC72,
    $11834, $13874, $178F4, $178F2, $10540, $10520, $18298, $10510, $10508,
    $10504, $105B0, $10598, $1058C, $10586, $105DC, $105CE, $186A0, $18690,
    $1C34C, $18688, $1C346, $18684, $18682, $104A0, $18258, $10DA0, $186D8,
    $1824C, $10D90, $186CC, $10D88, $186C6, $10D84, $10482, $10D82, $104D8,
    $1826E, $10DD8, $186EE, $10DCC, $104C6, $10DC6, $104EE, $10DEE, $1C750,
    $1C748, $1C744, $1C742, $18650, $18ED0, $1C76C, $1C326, $18EC8, $1C766,
    $18EC4, $18642, $18EC2, $10450, $10CD0, $10448, $18226, $11DD0, $10CC8,
    $10444, $11DC8, $10CC4, $10442, $11DC4, $10CC2, $1046C, $10CEC, $10466,
    $11DEC, $10CE6, $11DE6, $1E7A8, $1E7A4, $1E7A2, $1C728, $1CF68, $1E7B6,
    $1CF64, $1C722, $1CF62, $18628, $1C316, $18E68, $1C736, $19EE8, $18E64,
    $18622, $19EE4, $18E62, $19EE2, $10428, $18216, $10C68, $18636, $11CE8,
    $10C64, $10422, $13DE8, $11CE4, $10C62, $13DE4, $11CE2, $10436, $10C76,
    $11CF6, $13DF6, $1F7D4, $1F7D2, $1E794, $1EFB4, $1E792, $1EFB2, $1C714,
    $1CF34, $1C712, $1DF74, $1CF32, $1DF72, $18614, $18E34, $18612, $19E74,
    $18E32, $1BEF4),
    ($1F560, $1FAB8, $1EA40, $1F530, $1FA9C, $1EA20, $1F518, $1FA8E, $1EA10,
    $1F50C, $1EA08, $1F506, $1EA04, $1EB60, $1F5B8, $1FADE, $1D640, $1EB30,
    $1F59C, $1D620, $1EB18, $1F58E, $1D610, $1EB0C, $1D608, $1EB06, $1D604,
    $1D760, $1EBB8, $1F5DE, $1AE40, $1D730, $1EB9C, $1AE20, $1D718, $1EB8E,
    $1AE10, $1D70C, $1AE08, $1D706, $1AE04, $1AF60, $1D7B8, $1EBDE, $15E40,
    $1AF30, $1D79C, $15E20, $1AF18, $1D78E, $15E10, $1AF0C, $15E08, $1AF06,
    $15F60, $1AFB8, $1D7DE, $15F30, $1AF9C, $15F18, $1AF8E, $15F0C, $15FB8,
    $1AFDE, $15F9C, $15F8E, $1E940, $1F4B0, $1FA5C, $1E920, $1F498, $1FA4E,
    $1E910, $1F48C, $1E908, $1F486, $1E904, $1E902, $1D340, $1E9B0, $1F4DC,
    $1D320, $1E998, $1F4CE, $1D310, $1E98C, $1D308, $1E986, $1D304, $1D302,
    $1A740, $1D3B0, $1E9DC, $1A720, $1D398, $1E9CE, $1A710, $1D38C, $1A708,
    $1D386, $1A704, $1A702, $14F40, $1A7B0, $1D3DC, $14F20, $1A798, $1D3CE,
    $14F10, $1A78C, $14F08, $1A786, $14F04, $14FB0, $1A7DC, $14F98, $1A7CE,
    $14F8C, $14F86, $14FDC, $14FCE, $1E8A0, $1F458, $1FA2E, $1E890, $1F44C,
    $1E888, $1F446, $1E884, $1E882, $1D1A0, $1E8D8, $1F46E, $1D190, $1E8CC,
    $1D188, $1E8C6, $1D184, $1D182, $1A3A0, $1D1D8, $1E8EE, $1A390, $1D1CC,
    $1A388, $1D1C6, $1A384, $1A382, $147A0, $1A3D8, $1D1EE, $14790, $1A3CC,
    $14788, $1A3C6, $14784, $14782, $147D8, $1A3EE, $147CC, $147C6, $147EE,
    $1E850, $1F42C, $1E848, $1F426, $1E844, $1E842, $1D0D0, $1E86C, $1D0C8,
    $1E866, $1D0C4, $1D0C2, $1A1D0, $1D0EC, $1A1C8, $1D0E6, $1A1C4, $1A1C2,
    $143D0, $1A1EC, $143C8, $1A1E6, $143C4, $143C2, $143EC, $143E6, $1E828,
    $1F416, $1E824, $1E822, $1D068, $1E836, $1D064, $1D062, $1A0E8, $1D076,
    $1A0E4, $1A0E2, $141E8, $1A0F6, $141E4, $141E2, $1E814, $1E812, $1D034,
    $1D032, $1A074, $1A072, $1E540, $1F2B0, $1F95C, $1E520, $1F298, $1F94E,
    $1E510, $1F28C, $1E508, $1F286, $1E504, $1E502, $1CB40, $1E5B0, $1F2DC,
    $1CB20, $1E598, $1F2CE, $1CB10, $1E58C, $1CB08, $1E586, $1CB04, $1CB02,
    $19740, $1CBB0, $1E5DC, $19720, $1CB98, $1E5CE, $19710, $1CB8C, $19708,
    $1CB86, $19704, $19702, $12F40, $197B0, $1CBDC, $12F20, $19798, $1CBCE,
    $12F10, $1978C, $12F08, $19786, $12F04, $12FB0, $197DC, $12F98, $197CE,
    $12F8C, $12F86, $12FDC, $12FCE, $1F6A0, $1FB58, $16BF0, $1F690, $1FB4C,
    $169F8, $1F688, $1FB46, $168FC, $1F684, $1F682, $1E4A0, $1F258, $1F92E,
    $1EDA0, $1E490, $1FB6E, $1ED90, $1F6CC, $1F246, $1ED88, $1E484, $1ED84,
    $1E482, $1ED82, $1C9A0, $1E4D8, $1F26E, $1DBA0, $1C990, $1E4CC, $1DB90,
    $1EDCC, $1E4C6, $1DB88, $1C984, $1DB84, $1C982, $1DB82, $193A0, $1C9D8,
    $1E4EE, $1B7A0, $19390, $1C9CC, $1B790, $1DBCC, $1C9C6, $1B788, $19384,
    $1B784, $19382, $1B782, $127A0, $193D8, $1C9EE, $16FA0, $12790, $193CC,
    $16F90, $1B7CC, $193C6, $16F88, $12784, $16F84, $12782, $127D8, $193EE,
    $16FD8, $127CC, $16FCC, $127C6, $16FC6, $127EE, $1F650, $1FB2C, $165F8,
    $1F648, $1FB26, $164FC, $1F644, $1647E, $1F642, $1E450, $1F22C, $1ECD0,
    $1E448, $1F226, $1ECC8, $1F666, $1ECC4, $1E442, $1ECC2, $1C8D0, $1E46C,
    $1D9D0, $1C8C8, $1E466, $1D9C8, $1ECE6, $1D9C4, $1C8C2, $1D9C2, $191D0,
    $1C8EC, $1B3D0, $191C8, $1C8E6, $1B3C8, $1D9E6, $1B3C4, $191C2, $1B3C2,
    $123D0, $191EC, $167D0, $123C8, $191E6, $167C8, $1B3E6, $167C4, $123C2,
    $167C2, $123EC, $167EC, $123E6, $167E6, $1F628, $1FB16, $162FC, $1F624,
    $1627E, $1F622, $1E428, $1F216, $1EC68, $1F636, $1EC64, $1E422, $1EC62,
    $1C868, $1E436, $1D8E8, $1C864, $1D8E4, $1C862, $1D8E2, $190E8, $1C876,
    $1B1E8, $1D8F6, $1B1E4, $190E2, $1B1E2, $121E8, $190F6, $163E8, $121E4,
    $163E4, $121E2, $163E2, $121F6, $163F6, $1F614, $1617E, $1F612, $1E414,
    $1EC34, $1E412, $1EC32, $1C834, $1D874, $1C832, $1D872, $19074, $1B0F4,
    $19072, $1B0F2, $120F4, $161F4, $120F2, $161F2, $1F60A, $1E40A, $1EC1A,
    $1C81A, $1D83A, $1903A, $1B07A, $1E2A0, $1F158, $1F8AE, $1E290, $1F14C,
    $1E288, $1F146, $1E284, $1E282, $1C5A0, $1E2D8, $1F16E, $1C590, $1E2CC,
    $1C588, $1E2C6, $1C584, $1C582, $18BA0, $1C5D8, $1E2EE, $18B90, $1C5CC,
    $18B88, $1C5C6, $18B84, $18B82, $117A0, $18BD8, $1C5EE, $11790, $18BCC,
    $11788, $18BC6, $11784, $11782, $117D8, $18BEE, $117CC, $117C6, $117EE,
    $1F350, $1F9AC, $135F8, $1F348, $1F9A6, $134FC, $1F344, $1347E, $1F342,
    $1E250, $1F12C, $1E6D0, $1E248, $1F126, $1E6C8, $1F366, $1E6C4, $1E242,
    $1E6C2, $1C4D0, $1E26C, $1CDD0, $1C4C8, $1E266, $1CDC8, $1E6E6, $1CDC4,
    $1C4C2, $1CDC2, $189D0, $1C4EC, $19BD0, $189C8, $1C4E6, $19BC8, $1CDE6,
    $19BC4, $189C2, $19BC2, $113D0, $189EC, $137D0, $113C8, $189E6, $137C8,
    $19BE6, $137C4, $113C2, $137C2, $113EC, $137EC, $113E6, $137E6, $1FBA8,
    $175F0, $1BAFC, $1FBA4, $174F8, $1BA7E, $1FBA2, $1747C, $1743E, $1F328,
    $1F996, $132FC, $1F768, $1FBB6, $176FC, $1327E, $1F764, $1F322, $1767E,
    $1F762, $1E228, $1F116, $1E668, $1E224, $1EEE8, $1F776, $1E222, $1EEE4,
    $1E662, $1EEE2, $1C468, $1E236, $1CCE8, $1C464, $1DDE8, $1CCE4, $1C462,
    $1DDE4, $1CCE2, $1DDE2, $188E8, $1C476, $199E8, $188E4, $1BBE8, $199E4,
    $188E2, $1BBE4, $199E2, $1BBE2, $111E8, $188F6, $133E8, $111E4, $177E8,
    $133E4, $111E2, $177E4, $133E2, $177E2, $111F6, $133F6, $1FB94, $172F8,
    $1B97E, $1FB92, $1727C, $1723E, $1F314, $1317E, $1F734, $1F312, $1737E,
    $1F732, $1E214, $1E634, $1E212, $1EE74, $1E632, $1EE72, $1C434, $1CC74,
    $1C432, $1DCF4, $1CC72, $1DCF2, $18874, $198F4, $18872, $1B9F4, $198F2,
    $1B9F2, $110F4, $131F4, $110F2, $173F4, $131F2, $173F2, $1FB8A, $1717C,
    $1713E, $1F30A, $1F71A, $1E20A, $1E61A, $1EE3A, $1C41A, $1CC3A, $1DC7A,
    $1883A, $1987A, $1B8FA, $1107A, $130FA, $171FA, $170BE, $1E150, $1F0AC,
    $1E148, $1F0A6, $1E144, $1E142, $1C2D0, $1E16C, $1C2C8, $1E166, $1C2C4,
    $1C2C2, $185D0, $1C2EC, $185C8, $1C2E6, $185C4, $185C2, $10BD0, $185EC,
    $10BC8, $185E6, $10BC4, $10BC2, $10BEC, $10BE6, $1F1A8, $1F8D6, $11AFC,
    $1F1A4, $11A7E, $1F1A2, $1E128, $1F096, $1E368, $1E124, $1E364, $1E122,
    $1E362, $1C268, $1E136, $1C6E8, $1C264, $1C6E4, $1C262, $1C6E2, $184E8,
    $1C276, $18DE8, $184E4, $18DE4, $184E2, $18DE2, $109E8, $184F6, $11BE8,
    $109E4, $11BE4, $109E2, $11BE2, $109F6, $11BF6, $1F9D4, $13AF8, $19D7E,
    $1F9D2, $13A7C, $13A3E, $1F194, $1197E, $1F3B4, $1F192, $13B7E, $1F3B2,
    $1E114, $1E334, $1E112, $1E774, $1E332, $1E772, $1C234, $1C674, $1C232,
    $1CEF4, $1C672, $1CEF2, $18474, $18CF4, $18472, $19DF4, $18CF2, $19DF2,
    $108F4, $119F4, $108F2, $13BF4, $119F2, $13BF2, $17AF0, $1BD7C, $17A78,
    $1BD3E, $17A3C, $17A1E, $1F9CA, $1397C, $1FBDA, $17B7C, $1393E, $17B3E,
    $1F18A, $1F39A, $1F7BA, $1E10A, $1E31A, $1E73A, $1EF7A, $1C21A, $1C63A,
    $1CE7A, $1DEFA, $1843A, $18C7A, $19CFA, $1BDFA, $1087A, $118FA, $139FA,
    $17978, $1BCBE, $1793C, $1791E, $138BE, $179BE, $178BC, $1789E, $1785E,
    $1E0A8, $1E0A4, $1E0A2, $1C168, $1E0B6, $1C164, $1C162, $182E8, $1C176,
    $182E4, $182E2, $105E8, $182F6, $105E4, $105E2, $105F6, $1F0D4, $10D7E,
    $1F0D2, $1E094, $1E1B4, $1E092, $1E1B2, $1C134, $1C374, $1C132, $1C372,
    $18274, $186F4, $18272, $186F2, $104F4, $10DF4, $104F2, $10DF2, $1F8EA,
    $11D7C, $11D3E, $1F0CA, $1F1DA, $1E08A, $1E19A, $1E3BA, $1C11A, $1C33A,
    $1C77A, $1823A, $1867A, $18EFA, $1047A, $10CFA, $11DFA, $13D78, $19EBE,
    $13D3C, $13D1E, $11CBE, $13DBE, $17D70, $1BEBC, $17D38, $1BE9E, $17D1C,
    $17D0E, $13CBC, $17DBC, $13C9E, $17D9E, $17CB8, $1BE5E, $17C9C, $17C8E,
    $13C5E, $17CDE, $17C5C, $17C4E, $17C2E, $1C0B4, $1C0B2, $18174, $18172,
    $102F4, $102F2, $1E0DA, $1C09A, $1C1BA, $1813A, $1837A, $1027A, $106FA,
    $10EBE, $11EBC, $11E9E, $13EB8, $19F5E, $13E9C, $13E8E, $11E5E, $13EDE,
    $17EB0, $1BF5C, $17E98, $1BF4E, $17E8C, $17E86, $13E5C, $17EDC, $13E4E,
    $17ECE, $17E58, $1BF2E, $17E4C, $17E46, $13E2E, $17E6E, $17E2C, $17E26,
    $10F5E, $11F5C, $11F4E, $13F58, $19FAE, $13F4C, $13F46, $11F2E, $13F6E,
    $13F2C, $13F26),
    ($1ABE0, $1D5F8, $153C0, $1A9F0, $1D4FC, $151E0, $1A8F8, $1D47E, $150F0,
    $1A87C, $15078, $1FAD0, $15BE0, $1ADF8, $1FAC8, $159F0, $1ACFC, $1FAC4,
    $158F8, $1AC7E, $1FAC2, $1587C, $1F5D0, $1FAEC, $15DF8, $1F5C8, $1FAE6,
    $15CFC, $1F5C4, $15C7E, $1F5C2, $1EBD0, $1F5EC, $1EBC8, $1F5E6, $1EBC4,
    $1EBC2, $1D7D0, $1EBEC, $1D7C8, $1EBE6, $1D7C4, $1D7C2, $1AFD0, $1D7EC,
    $1AFC8, $1D7E6, $1AFC4, $14BC0, $1A5F0, $1D2FC, $149E0, $1A4F8, $1D27E,
    $148F0, $1A47C, $14878, $1A43E, $1483C, $1FA68, $14DF0, $1A6FC, $1FA64,
    $14CF8, $1A67E, $1FA62, $14C7C, $14C3E, $1F4E8, $1FA76, $14EFC, $1F4E4,
    $14E7E, $1F4E2, $1E9E8, $1F4F6, $1E9E4, $1E9E2, $1D3E8, $1E9F6, $1D3E4,
    $1D3E2, $1A7E8, $1D3F6, $1A7E4, $1A7E2, $145E0, $1A2F8, $1D17E, $144F0,
    $1A27C, $14478, $1A23E, $1443C, $1441E, $1FA34, $146F8, $1A37E, $1FA32,
    $1467C, $1463E, $1F474, $1477E, $1F472, $1E8F4, $1E8F2, $1D1F4, $1D1F2,
    $1A3F4, $1A3F2, $142F0, $1A17C, $14278, $1A13E, $1423C, $1421E, $1FA1A,
    $1437C, $1433E, $1F43A, $1E87A, $1D0FA, $14178, $1A0BE, $1413C, $1411E,
    $141BE, $140BC, $1409E, $12BC0, $195F0, $1CAFC, $129E0, $194F8, $1CA7E,
    $128F0, $1947C, $12878, $1943E, $1283C, $1F968, $12DF0, $196FC, $1F964,
    $12CF8, $1967E, $1F962, $12C7C, $12C3E, $1F2E8, $1F976, $12EFC, $1F2E4,
    $12E7E, $1F2E2, $1E5E8, $1F2F6, $1E5E4, $1E5E2, $1CBE8, $1E5F6, $1CBE4,
    $1CBE2, $197E8, $1CBF6, $197E4, $197E2, $1B5E0, $1DAF8, $1ED7E, $169C0,
    $1B4F0, $1DA7C, $168E0, $1B478, $1DA3E, $16870, $1B43C, $16838, $1B41E,
    $1681C, $125E0, $192F8, $1C97E, $16DE0, $124F0, $1927C, $16CF0, $1B67C,
    $1923E, $16C78, $1243C, $16C3C, $1241E, $16C1E, $1F934, $126F8, $1937E,
    $1FB74, $1F932, $16EF8, $1267C, $1FB72, $16E7C, $1263E, $16E3E, $1F274,
    $1277E, $1F6F4, $1F272, $16F7E, $1F6F2, $1E4F4, $1EDF4, $1E4F2, $1EDF2,
    $1C9F4, $1DBF4, $1C9F2, $1DBF2, $193F4, $193F2, $165C0, $1B2F0, $1D97C,
    $164E0, $1B278, $1D93E, $16470, $1B23C, $16438, $1B21E, $1641C, $1640E,
    $122F0, $1917C, $166F0, $12278, $1913E, $16678, $1B33E, $1663C, $1221E,
    $1661E, $1F91A, $1237C, $1FB3A, $1677C, $1233E, $1673E, $1F23A, $1F67A,
    $1E47A, $1ECFA, $1C8FA, $1D9FA, $191FA, $162E0, $1B178, $1D8BE, $16270,
    $1B13C, $16238, $1B11E, $1621C, $1620E, $12178, $190BE, $16378, $1213C,
    $1633C, $1211E, $1631E, $121BE, $163BE, $16170, $1B0BC, $16138, $1B09E,
    $1611C, $1610E, $120BC, $161BC, $1209E, $1619E, $160B8, $1B05E, $1609C,
    $1608E, $1205E, $160DE, $1605C, $1604E, $115E0, $18AF8, $1C57E, $114F0,
    $18A7C, $11478, $18A3E, $1143C, $1141E, $1F8B4, $116F8, $18B7E, $1F8B2,
    $1167C, $1163E, $1F174, $1177E, $1F172, $1E2F4, $1E2F2, $1C5F4, $1C5F2,
    $18BF4, $18BF2, $135C0, $19AF0, $1CD7C, $134E0, $19A78, $1CD3E, $13470,
    $19A3C, $13438, $19A1E, $1341C, $1340E, $112F0, $1897C, $136F0, $11278,
    $1893E, $13678, $19B3E, $1363C, $1121E, $1361E, $1F89A, $1137C, $1F9BA,
    $1377C, $1133E, $1373E, $1F13A, $1F37A, $1E27A, $1E6FA, $1C4FA, $1CDFA,
    $189FA, $1BAE0, $1DD78, $1EEBE, $174C0, $1BA70, $1DD3C, $17460, $1BA38,
    $1DD1E, $17430, $1BA1C, $17418, $1BA0E, $1740C, $132E0, $19978, $1CCBE,
    $176E0, $13270, $1993C, $17670, $1BB3C, $1991E, $17638, $1321C, $1761C,
    $1320E, $1760E, $11178, $188BE, $13378, $1113C, $17778, $1333C, $1111E,
    $1773C, $1331E, $1771E, $111BE, $133BE, $177BE, $172C0, $1B970, $1DCBC,
    $17260, $1B938, $1DC9E, $17230, $1B91C, $17218, $1B90E, $1720C, $17206,
    $13170, $198BC, $17370, $13138, $1989E, $17338, $1B99E, $1731C, $1310E,
    $1730E, $110BC, $131BC, $1109E, $173BC, $1319E, $1739E, $17160, $1B8B8,
    $1DC5E, $17130, $1B89C, $17118, $1B88E, $1710C, $17106, $130B8, $1985E,
    $171B8, $1309C, $1719C, $1308E, $1718E, $1105E, $130DE, $171DE, $170B0,
    $1B85C, $17098, $1B84E, $1708C, $17086, $1305C, $170DC, $1304E, $170CE,
    $17058, $1B82E, $1704C, $17046, $1302E, $1706E, $1702C, $17026, $10AF0,
    $1857C, $10A78, $1853E, $10A3C, $10A1E, $10B7C, $10B3E, $1F0BA, $1E17A,
    $1C2FA, $185FA, $11AE0, $18D78, $1C6BE, $11A70, $18D3C, $11A38, $18D1E,
    $11A1C, $11A0E, $10978, $184BE, $11B78, $1093C, $11B3C, $1091E, $11B1E,
    $109BE, $11BBE, $13AC0, $19D70, $1CEBC, $13A60, $19D38, $1CE9E, $13A30,
    $19D1C, $13A18, $19D0E, $13A0C, $13A06, $11970, $18CBC, $13B70, $11938,
    $18C9E, $13B38, $1191C, $13B1C, $1190E, $13B0E, $108BC, $119BC, $1089E,
    $13BBC, $1199E, $13B9E, $1BD60, $1DEB8, $1EF5E, $17A40, $1BD30, $1DE9C,
    $17A20, $1BD18, $1DE8E, $17A10, $1BD0C, $17A08, $1BD06, $17A04, $13960,
    $19CB8, $1CE5E, $17B60, $13930, $19C9C, $17B30, $1BD9C, $19C8E, $17B18,
    $1390C, $17B0C, $13906, $17B06, $118B8, $18C5E, $139B8, $1189C, $17BB8,
    $1399C, $1188E, $17B9C, $1398E, $17B8E, $1085E, $118DE, $139DE, $17BDE,
    $17940, $1BCB0, $1DE5C, $17920, $1BC98, $1DE4E, $17910, $1BC8C, $17908,
    $1BC86, $17904, $17902, $138B0, $19C5C, $179B0, $13898, $19C4E, $17998,
    $1BCCE, $1798C, $13886, $17986, $1185C, $138DC, $1184E, $179DC, $138CE,
    $179CE, $178A0, $1BC58, $1DE2E, $17890, $1BC4C, $17888, $1BC46, $17884,
    $17882, $13858, $19C2E, $178D8, $1384C, $178CC, $13846, $178C6, $1182E,
    $1386E, $178EE, $17850, $1BC2C, $17848, $1BC26, $17844, $17842, $1382C,
    $1786C, $13826, $17866, $17828, $1BC16, $17824, $17822, $13816, $17836,
    $10578, $182BE, $1053C, $1051E, $105BE, $10D70, $186BC, $10D38, $1869E,
    $10D1C, $10D0E, $104BC, $10DBC, $1049E, $10D9E, $11D60, $18EB8, $1C75E,
    $11D30, $18E9C, $11D18, $18E8E, $11D0C, $11D06, $10CB8, $1865E, $11DB8,
    $10C9C, $11D9C, $10C8E, $11D8E, $1045E, $10CDE, $11DDE, $13D40, $19EB0,
    $1CF5C, $13D20, $19E98, $1CF4E, $13D10, $19E8C, $13D08, $19E86, $13D04,
    $13D02, $11CB0, $18E5C, $13DB0, $11C98, $18E4E, $13D98, $19ECE, $13D8C,
    $11C86, $13D86, $10C5C, $11CDC, $10C4E, $13DDC, $11CCE, $13DCE, $1BEA0,
    $1DF58, $1EFAE, $1BE90, $1DF4C, $1BE88, $1DF46, $1BE84, $1BE82, $13CA0,
    $19E58, $1CF2E, $17DA0, $13C90, $19E4C, $17D90, $1BECC, $19E46, $17D88,
    $13C84, $17D84, $13C82, $17D82, $11C58, $18E2E, $13CD8, $11C4C, $17DD8,
    $13CCC, $11C46, $17DCC, $13CC6, $17DC6, $10C2E, $11C6E, $13CEE, $17DEE,
    $1BE50, $1DF2C, $1BE48, $1DF26, $1BE44, $1BE42, $13C50, $19E2C, $17CD0,
    $13C48, $19E26, $17CC8, $1BE66, $17CC4, $13C42, $17CC2, $11C2C, $13C6C,
    $11C26, $17CEC, $13C66, $17CE6, $1BE28, $1DF16, $1BE24, $1BE22, $13C28,
    $19E16, $17C68, $13C24, $17C64, $13C22, $17C62, $11C16, $13C36, $17C76,
    $1BE14, $1BE12, $13C14, $17C34, $13C12, $17C32, $102BC, $1029E, $106B8,
    $1835E, $1069C, $1068E, $1025E, $106DE, $10EB0, $1875C, $10E98, $1874E,
    $10E8C, $10E86, $1065C, $10EDC, $1064E, $10ECE, $11EA0, $18F58, $1C7AE,
    $11E90, $18F4C, $11E88, $18F46, $11E84, $11E82, $10E58, $1872E, $11ED8,
    $18F6E, $11ECC, $10E46, $11EC6, $1062E, $10E6E, $11EEE, $19F50, $1CFAC,
    $19F48, $1CFA6, $19F44, $19F42, $11E50, $18F2C, $13ED0, $19F6C, $18F26,
    $13EC8, $11E44, $13EC4, $11E42, $13EC2, $10E2C, $11E6C, $10E26, $13EEC,
    $11E66, $13EE6, $1DFA8, $1EFD6, $1DFA4, $1DFA2, $19F28, $1CF96, $1BF68,
    $19F24, $1BF64, $19F22, $1BF62, $11E28, $18F16, $13E68, $11E24, $17EE8,
    $13E64, $11E22, $17EE4, $13E62, $17EE2, $10E16, $11E36, $13E76, $17EF6,
    $1DF94, $1DF92, $19F14, $1BF34, $19F12, $1BF32, $11E14, $13E34, $11E12,
    $17E74, $13E32, $17E72, $1DF8A, $19F0A, $1BF1A, $11E0A, $13E1A, $17E3A,
    $1035C, $1034E, $10758, $183AE, $1074C, $10746, $1032E, $1076E, $10F50,
    $187AC, $10F48, $187A6, $10F44, $10F42, $1072C, $10F6C, $10726, $10F66,
    $18FA8, $1C7D6, $18FA4, $18FA2, $10F28, $18796, $11F68, $18FB6, $11F64,
    $10F22, $11F62, $10716, $10F36, $11F76, $1CFD4, $1CFD2, $18F94, $19FB4,
    $18F92, $19FB2, $10F14, $11F34, $10F12, $13F74, $11F32, $13F72, $1CFCA,
    $18F8A, $19F9A, $10F0A, $11F1A, $13F3A, $103AC, $103A6, $107A8, $183D6,
    $107A4, $107A2, $10396, $107B6, $187D4, $187D2, $10794, $10FB4, $10792,
    $10FB2, $1C7EA));

const
  TStPDF417TextCompaction: array[0..127] of TStPDF417TextCompactionData =
  (
    (Value: - 1; Mode: []), { 000 }
    (Value: - 1; Mode: []), { 001 }
    (Value: - 1; Mode: []), { 002 }
    (Value: - 1; Mode: []), { 003 }
    (Value: - 1; Mode: []), { 004 }
    (Value: - 1; Mode: []), { 005 }
    (Value: - 1; Mode: []), { 006 }
    (Value: - 1; Mode: []), { 007 }
    (Value: - 1; Mode: []), { 008 }
    (Value: 12; Mode: [cmMixed, cmPunctuation]), { 009 }
    (Value: 15; Mode: [cmPunctuation]), { 010 }
    (Value: - 1; Mode: []), { 011 }
    (Value: - 1; Mode: []), { 012 }
    (Value: 11; Mode: [cmMixed, cmPunctuation]), { 013 }
    (Value: - 1; Mode: []), { 014 }
    (Value: - 1; Mode: []), { 015 }
    (Value: - 1; Mode: []), { 016 }
    (Value: - 1; Mode: []), { 017 }
    (Value: - 1; Mode: []), { 018 }
    (Value: - 1; Mode: []), { 019 }
    (Value: - 1; Mode: []), { 020 }
    (Value: - 1; Mode: []), { 021 }
    (Value: - 1; Mode: []), { 022 }
    (Value: - 1; Mode: []), { 023 }
    (Value: - 1; Mode: []), { 024 }
    (Value: - 1; Mode: []), { 025 }
    (Value: - 1; Mode: []), { 026 }
    (Value: - 1; Mode: []), { 027 }
    (Value: - 1; Mode: []), { 028 }
    (Value: - 1; Mode: []), { 029 }
    (Value: - 1; Mode: []), { 030 }
    (Value: - 1; Mode: []), { 031 }
    (Value: 26; Mode: [cmAlpha, cmLower, cmMixed]), { 032 }
    (Value: 10; Mode: [cmPunctuation]), { 033 }
    (Value: 20; Mode: [cmPunctuation]), { 034 }
    (Value: 15; Mode: [cmMixed]), { 035 }
    (Value: 18; Mode: [cmMixed, cmPunctuation]), { 036 }
    (Value: 21; Mode: [cmMixed]), { 037 }
    (Value: 10; Mode: [cmMixed]), { 038 }
    (Value: 28; Mode: [cmPunctuation]), { 039 }
    (Value: 23; Mode: [cmPunctuation]), { 040 }
    (Value: 24; Mode: [cmPunctuation]), { 041 }
    (Value: 22; Mode: [cmMixed, cmPunctuation]), { 042 }
    (Value: 20; Mode: [cmMixed]), { 043 }
    (Value: 13; Mode: [cmMixed, cmPunctuation]), { 044 }
    (Value: 16; Mode: [cmMixed, cmPunctuation]), { 045 }
    (Value: 17; Mode: [cmMixed, cmPunctuation]), { 046 }
    (Value: 19; Mode: [cmMixed, cmPunctuation]), { 047 }
    (Value: 0; Mode: [cmMixed]), { 048 }
    (Value: 1; Mode: [cmMixed]), { 049 }
    (Value: 2; Mode: [cmMixed]), { 050 }
    (Value: 3; Mode: [cmMixed]), { 051 }
    (Value: 4; Mode: [cmMixed]), { 052 }
    (Value: 5; Mode: [cmMixed]), { 053 }
    (Value: 6; Mode: [cmMixed]), { 054 }
    (Value: 7; Mode: [cmMixed]), { 055 }
    (Value: 8; Mode: [cmMixed]), { 056 }
    (Value: 9; Mode: [cmMixed]), { 057 }
    (Value: 14; Mode: [cmMixed, cmPunctuation]), { 058 }
    (Value: 0; Mode: [cmPunctuation]), { 059 }
    (Value: 1; Mode: [cmPunctuation]), { 060 }
    (Value: 23; Mode: [cmMixed]), { 061 }
    (Value: 2; Mode: [cmPunctuation]), { 062 }
    (Value: 25; Mode: [cmPunctuation]), { 063 }
    (Value: 3; Mode: [cmPunctuation]), { 064 }
    (Value: 0; Mode: [cmAlpha]), { 065 }
    (Value: 1; Mode: [cmAlpha]), { 066 }
    (Value: 2; Mode: [cmAlpha]), { 067 }
    (Value: 3; Mode: [cmAlpha]), { 068 }
    (Value: 4; Mode: [cmAlpha]), { 069 }
    (Value: 5; Mode: [cmAlpha]), { 070 }
    (Value: 6; Mode: [cmAlpha]), { 071 }
    (Value: 7; Mode: [cmAlpha]), { 072 }
    (Value: 8; Mode: [cmAlpha]), { 073 }
    (Value: 9; Mode: [cmAlpha]), { 074 }
    (Value: 10; Mode: [cmAlpha]), { 075 }
    (Value: 11; Mode: [cmAlpha]), { 076 }
    (Value: 12; Mode: [cmAlpha]), { 077 }
    (Value: 13; Mode: [cmAlpha]), { 078 }
    (Value: 14; Mode: [cmAlpha]), { 079 }
    (Value: 15; Mode: [cmAlpha]), { 080 }
    (Value: 16; Mode: [cmAlpha]), { 081 }
    (Value: 17; Mode: [cmAlpha]), { 082 }
    (Value: 18; Mode: [cmAlpha]), { 083 }
    (Value: 19; Mode: [cmAlpha]), { 084 }
    (Value: 20; Mode: [cmAlpha]), { 085 }
    (Value: 21; Mode: [cmAlpha]), { 086 }
    (Value: 22; Mode: [cmAlpha]), { 087 }
    (Value: 23; Mode: [cmAlpha]), { 088 }
    (Value: 24; Mode: [cmAlpha]), { 089 }
    (Value: 25; Mode: [cmAlpha]), { 090 }
    (Value: 4; Mode: [cmPunctuation]), { 091 }
    (Value: 5; Mode: [cmPunctuation]), { 092 }
    (Value: 6; Mode: [cmPunctuation]), { 093 }
    (Value: 24; Mode: [cmMixed]), { 094 }
    (Value: 7; Mode: [cmPunctuation]), { 095 }
    (Value: 8; Mode: [cmPunctuation]), { 096 }
    (Value: 0; Mode: [cmLower]), { 097 }
    (Value: 1; Mode: [cmLower]), { 098 }
    (Value: 2; Mode: [cmLower]), { 099 }
    (Value: 3; Mode: [cmLower]), { 100 }
    (Value: 4; Mode: [cmLower]), { 101 }
    (Value: 5; Mode: [cmLower]), { 102 }
    (Value: 6; Mode: [cmLower]), { 103 }
    (Value: 7; Mode: [cmLower]), { 104 }
    (Value: 8; Mode: [cmLower]), { 105 }
    (Value: 9; Mode: [cmLower]), { 106 }
    (Value: 10; Mode: [cmLower]), { 107 }
    (Value: 11; Mode: [cmLower]), { 108 }
    (Value: 12; Mode: [cmLower]), { 109 }
    (Value: 13; Mode: [cmLower]), { 110 }
    (Value: 14; Mode: [cmLower]), { 111 }
    (Value: 15; Mode: [cmLower]), { 112 }
    (Value: 16; Mode: [cmLower]), { 113 }
    (Value: 17; Mode: [cmLower]), { 114 }
    (Value: 18; Mode: [cmLower]), { 115 }
    (Value: 19; Mode: [cmLower]), { 116 }
    (Value: 20; Mode: [cmLower]), { 117 }
    (Value: 21; Mode: [cmLower]), { 118 }
    (Value: 22; Mode: [cmLower]), { 119 }
    (Value: 23; Mode: [cmLower]), { 120 }
    (Value: 24; Mode: [cmLower]), { 121 }
    (Value: 25; Mode: [cmLower]), { 122 }
    (Value: 26; Mode: [cmPunctuation]), { 123 }
    (Value: 21; Mode: [cmPunctuation]), { 124 }
    (Value: 27; Mode: [cmPunctuation]), { 125 }
    (Value: 9; Mode: [cmPunctuation]), { 126 }
    (Value: - 1; Mode: []) { 127 }
  );

type

  TStBarKind = (bkSpace, bkBar, bkThreeQuarterBar, bkHalfBar, bkGuard, bkSupplement, bkBlankSpace);

  TStBarKindSet = set of TStBarKind;
  TStDigitArray = array[1..bcMaxBarCodeLen] of Byte;

  TBarData = class
    FKind: TStBarKindSet;
    FModules: Integer;
  public
    property Kind: TStBarKindSet read FKind write FKind;
    property Modules: Integer read FModules write FModules;
  end;

  TBarCodeInfo = class
  private
    FBars: TObjectList;
    function GetBars(Index: Integer): TBarData;
    function GetCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(ModuleCount: Integer; BarKind: TStBarKindSet);
    procedure Clear;
    property Bars[Index: Integer]: TBarData read GetBars; default;
    property Count: Integer read GetCount;
  end;

  TBarCodeType = (bcNone, bcUPC_A, bcUPC_E, bcEAN_8, bcEAN_13, bcInterleaved2of5, bcCodabar, bcCode11, bcCode39, bcCode93, bcCode128, bcPDF417);
  TCode128CodeSubset = (csCodeA, csCodeB, csCodeC);

  TBarcodeDrawFlag = (bfCenter, bfCalcRect, bfShowGuardChars, bfExtendedSyntax, bfGuardBarAbove, bfGuardBarBelow);
  TBarcodeDrawFlags = set of TBarcodeDrawFlag;

  TBarcodeDrawInfo = record
    Code: string;
    R: TRect;
    BarcodeType: TBarCodeType;
    BarColor: TColor;
    BarWidth: Integer;
    BarToSpaceRatio: Integer;
    ShowCode: Boolean;
    ShowGuardChars: Boolean;
    ExtendedSyntax: Boolean;
    BearerBars: Boolean;
    TallGuardBars: Boolean;
    SupplementalCode: string;
    BarNarrowToWideRatio: Integer;
    AddCheckChar: Boolean;
    Code128Subset: TCode128CodeSubset;
    //
    Center: Boolean;
    CalcRect: Boolean;
    GuardBarAbove: Boolean;
    GuardBarBelow: Boolean;
  end;

const
  BarcodeTypeNames: array[TBarCodeType] of string =
  (
    'None',
    'UPC_A',
    'UPC_E',
    'EAN_8',
    'EAN_13',
    'Interleaved2of5',
    'Codabar',
    'Code11',
    'Code39',
    'Code93',
    'Code128',
    'PDF 417'
    );

procedure GetCheckCharacters(const S: string; var C, K: Integer; BarcodeType: TBarcodeType; var Digits: TStDigitArray; var Count: integer);
function GetDigits(Characters: string; var Digits: TStDigitArray; BarcodeType: TBarcodeType; Code128Subset: TCode128CodeSubset = csCodeA): Integer;
procedure CalcBarCode(DrawInfo: TBarcodeDrawInfo; var Digits: TStDigitArray; var Info: TBarcodeInfo);
procedure RaiseError(E: ExceptClass; Msg: string);
procedure DrawBarcode(Canvas: TCanvas; DrawInfo: TBarcodeDrawInfo; Info: TBarcodeInfo; PDrawRect: PRect = nil);
procedure DrawPDF417Barcode(Canvas: TCanvas; DrawInfo: TBarcodeDrawInfo; Info: TBarcodeInfo; PDrawRect: PRect = nil);
function CorrectBarcodeText(const vText: string; DrawInfo: TBarcodeDrawInfo): string;

implementation

//*********************************************************

function IsNumericString(const S: string): boolean;
var
  i: integer;
  LenS: integer;
begin
  {note: an assertion test for ConvertToBase900}
  Result := false;
  LenS := length(S);
  if (LenS = 0) or (LenS > 45) then
    Exit;
  for i := 1 to LenS do
    if not (('0' <= S[i]) and (S[i] <= '9')) then
      Exit;
  Result := true;
end;

procedure ConvertBytesToBase900(const S: array of Byte; var A: array of Integer);
var
  i: Integer;
  D: array[0..5] of Byte;
  Dividend: Integer;
  Digits: array[0..4] of Integer;
  SP: Integer;

begin
  //  Assert(length(S) = 6,
  //    'ConvertBytesToBase900: there should be 6 bytes in the input byte array');
  //  Assert(length(A) = 5,
  //    'ConvertBytesToBase900: there should be 5 elements in the output digit array');

    {copy the array of bytes}
  for i := 0 to 5 do
    D[i] := S[i];

  {loop until the entire base 256 value has been converted to an array
   of base 900 digits (6 base 256 digits will convert to 5 base 900
   digits)}
  SP := 0;
  while (SP < 5) do
  begin
    Dividend := 0;
    for i := 0 to 5 do
    begin
      {notes: at the start of the loop, Dividend will always be in the
                range 0..899--it starts out as zero and the final
                statement in the loop forces it into that range
              the first calculation sets Dividend to 0..230399
              the second calc sets D[i] to 0..255 (with no possibility
                of overflow)
              the third calc sets Dividend to 0..899 again}
      Dividend := (Dividend shl 8) + D[i];
      D[i] := Dividend div 900;
      Dividend := Dividend mod 900;
    end;

    Digits[SP] := Dividend;
    inc(SP);
  end;

  {pop the base 900 digits and enter them into the array of integers}
  i := 0;
  while (SP > 0) do
  begin
    dec(SP);
    A[i] := Digits[SP];
    inc(i);
  end;
end;

function ConvertToBase900(const S: string; var A: array of Integer; var LenA: Integer): Boolean;
var
  D: string;
  i: Integer;
  LenD: Integer;
  Dividend: Integer;
  Rem: Integer;
  Done: Boolean;
  FirstDigit: Integer;
  Digits: array[0..14] of Integer;
  // 15 base 900 digits = 45 base 10 digits
  SP: Integer;

begin
  {Assert: S must be non-empty
           it must contain just the ASCII characters '0' to '9' (so no
             leading/trailing spaces either)
           it must have a maximum length of 45}
  //Assert(IsNumericString(S), 'ConvertToBase900: S should be a numeric string');
  Result := IsNumericString(S);
  if Result then
  begin

    {grab the string and calculate its length}
    D := S;
    LenD := length(D);

    {convert the string from ASCII characters into binary digits and in
     the process calculate the first non-zero digit}
    FirstDigit := 0;
    for i := LenD downto 1 do
    begin
      D[i] := char(ord(D[i]) - ord('0'));
      if (D[i] <> #0) then
        FirstDigit := i;
    end;

    {if the input string comprises just zero digits, return}
    if (FirstDigit = 0) then
    begin
      LenA := 0;
      Exit;
    end;

    {prepare the stack of base 900 digits}
    SP := 0;

    {loop until the entire base 10 string has been converted to an array
     of base 900 digits}
    Done := false;
    while not Done do
    begin

      {if we can switch to using standard integer arithmetic, do so}
      if ((LenD - FirstDigit) <= 8) then
      begin

        {convert the remaining digits to a binary integer}
        Dividend := 0;
        for i := FirstDigit to LenD do
          Dividend := (Dividend * 10) + ord(D[i]);

        {calculate the remaining base 900 digits using the standard
         radix conversion algorithm; push onto the digit stack}
        while (Dividend <> 0) do
        begin
          Digits[SP] := Dividend mod 900;
          inc(SP);
          Dividend := Dividend div 900;
        end;

        {we've finished}
        Done := true;
      end

        {otherwise operate directly on the base 10 string}
      else
      begin

        {calculate the remainder base 100}
        Rem := ord(D[LenD]);
        dec(LenD);
        Rem := Rem + (ord(D[LenD]) * 10);
        dec(LenD);

        {calculate the quotient and remainder of the remaining digits,
         dividing by 9}
        Dividend := 0;
        for i := FirstDigit to LenD do
        begin
          Dividend := (Dividend * 10) + ord(D[i]);
          D[i] := char(Dividend div 9);
          Dividend := Dividend mod 9;
        end;

        {push the base 900 digit onto the stack: it's the remainder base
         9 multiplied by 100, plus the remainder base 100}
        Digits[SP] := (Dividend * 100) + Rem;
        inc(SP);

        {if the first digit is now zero, advance the index to the first
         non-zero digit}
        if (D[FirstDigit] = '0') then
          inc(FirstDigit);
      end;
    end;

    {pop the base 900 digits and enter them into the array of integers}
    i := 0;
    while (SP > 0) do
    begin
      dec(SP);
      A[i] := Digits[SP];
      inc(i);
    end;
    LenA := i;
  end;
end;

function TextToCodewords(const Code: string; var vCount: Integer): TStPDF417CodewordList;
var
  //FNumCodewords: Integer; //need review belal
  FNewTextCodeword: Boolean;

  procedure AddCodeword(Value: Word);
  var
    l: Integer;
  begin
    l := Length(Result);
    if vCount >= l then
      SetLength(Result, l + 1000);
    Result[vCount] := Value;
    Inc(vCount);
  end;

  function GetNextCharacter(var NewChar: Integer; var Codeword: Boolean; var Position: Integer; CodeLen: Integer): Boolean;
  var
    WorkNum: Integer;

  begin
    NewChar := 0;
    Codeword := False;
    Result := True;

    if Position <= CodeLen then
    begin
      if (Code[Position] = '\') and
        (Position < CodeLen) then
      begin
        case Code[Position + 1] of
          '0'..'9':
            begin
              try
                NewChar := StrToInt(Copy(Code, Position + 1, 3));
                Inc(Position, 4);
              except
                NewChar := 0;
                Inc(Position, 4);
              end;
            end;
          'C', 'c':
            begin
              try
                Codeword := True;
                NewChar := StrToInt(Copy(Code, Position + 2, 3));
                Inc(Position, 5);
              except
                NewChar := 0;
                Inc(Position, 5);
              end;
            end;
          'G', 'g':
            begin
              WorkNum := StrToInt(Copy(Code, Position + 1, 6));
              Inc(Position, 8);
              if (WorkNum >= 0) and (WorkNum <= 899) then
              begin
                AddCodeword(927);
                Codeword := True;
                NewChar := WorkNum;
              end
              else if (WorkNum >= 900) and (WorkNum < 810900) then
              begin
                AddCodeword(926);
                AddCodeword((WorkNum div 900) - 1);
                Codeword := True;
                NewChar := WorkNum mod 900;
              end
              else if (WorkNum >= 810900) and (WorkNum < 811800) then
              begin
                AddCodeword(925);
                Codeword := True;
                NewChar := WorkNum;
              end
              else
                //raise EStBarCodeError.Create('StEGLIOutOfRange');
                Result := False;
            end;
          'X', 'x':
            begin
              try
                NewChar := StrToInt('$' + Copy(Code, Position + 2, 2));
                Inc(Position, 4);
              except
                NewChar := 0;
                Inc(Position, 4);
              end;
            end;
          '\':
            begin
              NewChar := Byte(Code[Position]);
              Inc(Position, 2);
            end;
        else
          begin
            NewChar := Byte(Code[Position]);
            Inc(Position);
          end;
        end;
      end
      else
      begin
        NewChar := Byte(Code[Position]);
        Inc(Position);
      end;
    end;
  end;

  function GoodForNumericCompaction(Position: Integer; CodeLen: Integer; var Count: Integer): Boolean;
  const
    BytesNeeded = 13;
  begin
    Result := False;
    Count := 0;
    while (Position + Count < CodeLen) and
      (Code[Position + Count] >= '0') and
      (Code[Position + Count] <= '9') do
      Inc(Count);
    if Count > BytesNeeded then
      Result := True;
  end;

  function GoodForTextCompaction(Position: Integer; CodeLen: Integer; var Count: Integer): Boolean;
    function IsGoodTextValue(const v: Char): Boolean; {!!.01}
    begin {!!.01}
      if v > #127 then {!!.01}
        Result := False {!!.01}
      else if TStPDF417TextCompaction[Integer(v)].Value >= 0 then {!!.01}
        Result := True {!!.01}
      else {!!.01}
        Result := False; {!!.01}
    end; {!!.01}
  const
    //BytesNeeded = 5;
    BytesNeeded = 10; //belal ?????????
  begin
    Result := False;
    Count := 0;
    while (Position + Count < CodeLen) and {!!.01}
    (IsGoodTextValue(Code[Position + Count])) and {!!.01}
    (Count <= BytesNeeded) do {!!.01}
      Inc(Count);
    if (Count > BytesNeeded) or
      ((Position + Count >= CodeLen) and (Count > 0)) then
      Result := True;
  end;

  procedure EncodeBinary(var Position: Integer; CodeLen: Integer);

    function CountBytes(Position: Integer; CodeLen: Integer): Integer;
    {var
      Done: Boolean;
      Dummy: Integer;}

    begin
      Result := CodeLen; //belal use Binary Encode for entire Code
      {Result := 0;
      Done := False;
      while not done do
      begin
        if (Result < CodeLen) and
          (not GoodForNumericCompaction(Position + Result, CodeLen, Dummy)) and
          (not GoodForTextCompaction(Position + Result, CodeLen, Dummy)) then
          Inc(Result)
        else
          Done := True;
      end;}
    end;

  var
    MultipleOfSix: Boolean;
    BinaryDataSize: Integer;
    i: Integer;
    j: Integer;
    A: array[0..6] of Integer;

  const
    Even6Bytes = 924;
    Odd6Bytes = 901;

  begin
    BinaryDataSize := CountBytes(Position, CodeLen);
    if BinaryDataSize mod 6 = 0 then
      MultipleOfSix := True
    else
      MultipleOfSix := False;
    if MultipleOfSix then
      AddCodeword(Even6Bytes)
    else
      AddCodeword(Odd6Bytes);

    i := 0;
    while i < BinaryDataSize do
      if BinaryDataSize - i < 6 then
      begin
        AddCodeword(Word(Code[Position + i]));
        Inc(i);
      end
      else
      begin
        ConvertBytesToBase900([Byte(Code[Position + i]),
          Byte(Code[Position + i + 1]),
            Byte(Code[Position + i + 2]),
            Byte(Code[Position + i + 3]),
            Byte(Code[Position + i + 4]),
            Byte(Code[Position + i + 5])], A);
        for j := 1 to 5 do
          AddCodeword(A[j - 1]); {!!.dg}
        Inc(i, 6);
      end;
    Inc(Position, BinaryDataSize); {!!.dg}
  end;

  function EncodeText(var Position: Integer; CodeLen: Integer; var vList: TStPDF417CodewordList): Boolean;

    function SelectBestTextMode(CurChar: TStPDF417TextCompactionData): TStPDF417TextCompactionMode;
    begin
      if cmAlpha in CurChar.Mode then
        Result := cmAlpha
      else if cmLower in CurChar.Mode then
        Result := cmLower
      else if cmMixed in CurChar.Mode then
        Result := cmMixed
      else if cmPunctuation in CurChar.Mode then
        Result := cmPunctuation
      else
        Result := cmNone;
    end;

    procedure AddTextCharacter(Value: Word);
    begin
      if FNewTextCodeword then
        vList[vCount] := 30 * Value
      else
      begin
        vList[vCount] := vList[vCount] + Value;
        Inc(vCount);
      end;
      FNewTextCodeword := not FNewTextCodeword;
    end;

    function ChangeTextSubmode(CurrentMode: TStPDF417TextCompactionMode; NewMode: TStPDF417TextCompactionMode; UseShift: Boolean): TStPDF417TextCompactionMode;
    const
      LatchAlphaToLower = 27;
      LatchAlphaToMixed = 28;
      ShiftAlphaToPunctuation = 29;
      ShiftLowerToAlpha = 27;
      LatchLowerToMixed = 28;
      ShiftLowertoPunctuation = 29;
      LatchMixedToPunctuation = 25;
      LatchMixedToLower = 27;
      LatchMixedToAlpha = 28;
      ShiftMixedToPunctuation = 29;
      LatchPunctuationToAlpha = 29;

    begin
      if UseShift then
        Result := CurrentMode
      else
        Result := NewMode;

      case CurrentMode of
        cmAlpha:
          case NewMode of
            cmLower:
              begin
                { Alpha to Lower.  No shift }
                AddTextCharacter(LatchAlphaToLower);
                if UseShift then
                  Result := NewMode;
              end;
            cmMixed:
              begin
                { Alpha to Numeric.  No shift }
                AddTextCharacter(LatchAlphaToMixed);
                if UseShift then
                  Result := NewMode;
              end;
            cmPunctuation:
              { Alpha to Punctuation }
              if UseShift then
                AddTextCharacter(ShiftAlphaToPunctuation)
              else
              begin
                AddTextCharacter(LatchAlphaToMixed);
                AddTextCharacter(LatchMixedToPunctuation);
              end;
          end;

        cmLower:
          case NewMode of
            cmAlpha:
              { Lower to Alpha }
              if UseShift then
                AddTextCharacter(ShiftLowerToAlpha)
              else
              begin
                AddTextCharacter(LatchLowerToMixed);
                AddTextCharacter(LatchMixedToAlpha);
              end;
            cmMixed:
              begin
                { Lower to Mixed.  No shift }
                AddTextCharacter(LatchLowerToMixed);
                if UseShift then
                  Result := NewMode;
              end;
            cmPunctuation:
              { Lower to Punctuation }
              if UseShift then
                AddTextCharacter(ShiftLowerToPunctuation)
              else
              begin
                AddTextCharacter(LatchLowerToMixed);
                AddTextCharacter(LatchMixedToPunctuation);
              end;
          end;

        cmMixed:
          case NewMode of
            cmAlpha:
              begin
                { Mixed to Alpha.  No shift }
                AddTextCharacter(LatchMixedToAlpha);
                if UseShift then
                  Result := NewMode;
              end;
            cmLower:
              begin
                { Mixed to Lower.  No shift }
                AddTextCharacter(LatchMixedToLower);
                if UseShift then
                  Result := NewMode;
              end;
            cmPunctuation:
              { Mixed to Punctuation }
              if UseShift then
                AddTextCharacter(ShiftMixedToPunctuation)
              else
                AddTextCharacter(LatchMixedToPunctuation);
          end;
        cmPunctuation:
          case NewMode of
            cmAlpha:
              begin
                { Punctuation to Alpha.  No shift }
                AddTextCharacter(LatchPunctuationToAlpha);
                if UseShift then
                  Result := NewMode;
              end;
            cmLower:
              begin
                { Punctuation to Lower.  No shift }
                AddTextCharacter(LatchPunctuationToAlpha);
                AddTextCharacter(LatchAlphaToLower);
                if UseShift then
                  Result := NewMode;
              end;
            cmMixed:
              begin
                { Punctuation to Mixed.  No shift }
                AddTextCharacter(LatchPunctuationToAlpha);
                AddTextCharacter(LatchAlphaToMixed);
                if UseShift then
                  Result := NewMode;
              end;
          end;
      end;
    end;

  var
    CurrentTextSubmode: TStPDF417TextCompactionMode;
    CurChar: TStPDF417TextCompactionData;
    UseShift: Boolean;
    Done: Boolean;
    Dummy: Integer;
    NewChar: Integer;
    Codeword: Boolean;

  const
    EndingPadChar = 29;

  begin
    { Initialize and get the first character }
    FNewTextCodeword := True;
    CurrentTextSubmode := cmAlpha;
    Done := False;
    Result := True;

    { get characters until it is necessary to step out of text mode }
    while (Position <= CodeLen) and (CurChar.Value >= 0) and not Done and Result do
    begin
      if (Position <= CodeLen) then
      begin
        Result := GetNextCharacter(NewChar, Codeword, Position, CodeLen);
        if Result then CurChar := TStPDF417TextCompaction[NewChar];
      end;
      if Result then //belal
      begin
        if Codeword then
        begin
          { If the text contains an odd number of letters, follow it with a
            trailing 29 }
          if not FNewTextCodeword then
            AddTextCharacter(EndingPadChar);
          FNewTextCodeword := True;
          { Add the codeword }
          AddCodeword(NewChar)
        end
        else
        begin
          { Check if the text submode for the current character is different than
            the current text submode }
          if not (CurrentTextSubmode in CurChar.Mode) then
          begin
            { if the text submode is different, see if it remains different.  If
              it does, use a latch, otherwise just shift }
            if Position < CodeLen then
            begin
              if not (CurrentTextSubmode in
                TStPDF417TextCompaction[Integer(Code[Position + 1])].Mode) then
                UseShift := False
              else
                UseShift := True;
            end
            else
              UseShift := True;

            { Add the shift or latch to the text codewords }
            CurrentTextSubmode := ChangeTextSubmode(CurrentTextSubmode,
              SelectBestTextMode(CurChar),
              UseShift);
          end;

          { Add the character to the codeword array }
          AddTextCharacter(CurChar.Value);
        end;
        { If this is a digit and it looks like a good time to switch to
          numeric mode, do so }
        if GoodForNumericCompaction(Position, CodeLen, Dummy) then
          Done := True;
      end;
    end;

    { If the text contains an odd number of letters, follow it with a
      trailing 29 }
    if Result and not FNewTextCodeword then
      AddTextCharacter(EndingPadChar);
  end;

  function EncodeNumeric(var Position: Integer; CodeLen: Integer): Boolean;
    function CollectDigits(var Position: Integer; CodeLen: Integer): string;
    var
      StartPos: Integer;
    const
      MaxDigitChunk = 44;
    begin
      Result := '';
      StartPos := Position;
      while (Position <= CodeLen) and (Position - StartPos < MaxDigitChunk) and
        (Code[Position] >= '0') and (Code[Position] <= '9') do
      begin
        Inc(Position);
      end;
      if Position - StartPos > 0 then
        Result := '1' + Copy(Code, StartPos, Position - StartPos);
    end;

  var
    NumericString: string;
    A: array[0..44] of Integer;
    LenA: Integer;
    i: Integer;

  const
    NumericLatch = 902;

  begin
    AddCodeword(NumericLatch);
    Result := True;
    repeat
      NumericString := CollectDigits(Position, CodeLen);
      if NumericString <> '' then
      begin
        Result := ConvertToBase900(NumericString, A, LenA);
        if Result then
          for i := 0 to LenA do
            AddCodeword(A[i]);
      end;
    until not Result or (NumericString = '');
  end;

var
  i: Integer;
  CodeLen: Integer;
  //CurrentMode: TStDataMode;
  //Count: Integer;
  //First: Boolean;
  aAccept: Boolean;

const
  TextCompaction = 900;
  PadCodeword = 900;

begin
  //First := True; //belal use Binary Encode for entire Code
  SetLength(Result, 2701);
  for i := 0 to 2700 do
    Result[i] := PadCodeword;
  vCount := 1; { There will always be a length codeword }
  i := 1;

  CodeLen := Length(Code);
  if CodeLen = 0 then
    Exit;

  {if GoodForNumericCompaction(i, CodeLen, Count) then //belal use Binary Encode for entire Code
    CurrentMode := dmNumeric
  else if GoodForTextCompaction(i, CodeLen, Count) then
    CurrentMode := dmText
  else
    CurrentMode := dmBinary;}
    
  aAccept := True;
  EncodeBinary(i, CodeLen);

  (*while aAccept and (i < CodeLen) do //belal use Binary Encode for entire Code
  begin
    case CurrentMode of
      dmBinary:
        EncodeBinary(i, CodeLen);
      dmText:
        if First then
          aAccept := EncodeText(i, CodeLen, Result);
      dmNumeric:
        aAccept := EncodeNumeric(i, CodeLen);
    end;

    if GoodForNumericCompaction(i, CodeLen, Count) then
      CurrentMode := dmNumeric
    else if GoodForTextCompaction(i, CodeLen, Count) then
    begin
      if not First then
        AddCodeword(TextCompaction);
      CurrentMode := dmText;
      aAccept := EncodeText(i, CodeLen,Result); {!!.01}
    end
    else
      CurrentMode := dmBinary;
    First := False;
  end;*)
  if not aAccept then Result := nil;
end;

procedure DrawPDF417Barcode(Canvas: TCanvas; DrawInfo: TBarcodeDrawInfo; Info: TBarcodeInfo; PDrawRect: PRect = nil);
var
  FHighlight: Boolean;
  Truncated: Boolean;
  FECCLevel: Integer;
  FCodeWords: TStPDF417CodewordList;
  NumColumns, NumRows: Integer;
  FNumCodeWords: Integer;

  function NumCodewords: Integer;
  begin
    Result := FNumCodeWords;
  end;

  function GetBarWidth: Integer;
  begin
    Result := DrawInfo.BarWidth div 10;
  end;

  function GetRealErrorLevel: Integer;
  begin
    if (FECCLevel < 0) then
    begin
      if NumCodeWords < 41 then
        Result := 2
      else if NumCodeWords < 161 then
        Result := 3
      else if NumCodeWords < 321 then
        Result := 4
      else
        Result := 5;
    end
    else
      Result := FECCLevel
  end;

  function CheckCodeword(Codeword: Integer): Boolean;
  begin
    Result := (Codeword >= 0) and (CodeWord <= 929);
  end;

  function CodewordToBitmask(RowNumber: Integer; Codeword: Integer):  DWORD;
  begin
    {if (Codeword < 0) or (CodeWord > 929) then
      raise EStBarCodeError.Create('StEInvalidCodeword');
    Result := StPDF417Codewords[RowNumber mod 3][Codeword];}
    //no need to check belal
    Result := StPDF417Codewords[RowNumber mod 3][Codeword]
  end;

  procedure DrawBar(X1, Y1, X2, Y2: Integer);
  var
    rc: TRect;
  begin
    rc := Rect(x1, y1, x2, y2);
    Canvas.FillRect(rc);
  end;

  procedure DrawCodeword(vRect: TRect; RowNumber: Integer; ColNumber: Integer; WorkBarHeight: Integer; Pattern: string);
    function GetColumnPosition(ColNumber: Integer): Integer;
    begin
      Result := ColNumber * StPDF417CellWidth * GetBarWidth;
    end;

  var
    i: Integer;
    CurPos: Integer;
    NewPos: Integer;
    DrawBlock: Boolean;

  begin
    if FHighlight then
    begin
      DrawBar(vRect.Left + (GetColumnPosition(ColNumber)), vRect.Top + RowNumber * WorkBarHeight, vRect.Left + 17 * GetBarWidth + GetColumnPosition(ColNumber), vRect.Top + (RowNumber + 1) * WorkBarHeight);
      Canvas.Brush.Color := DrawInfo.BarColor; // Color; belal
    end;

    CurPos := 0;
    DrawBlock := True;
    for i := 1 to Length(Pattern) do
    begin
      NewPos := StrToInt(Copy(Pattern, i, 1)) * GetBarWidth;
      if DrawBlock then
        //Canvas.Rectangle(vRect.Left + CurPos + GetColumnPosition (ColNumber), vRect.Top + RowNumber * WorkBarHeight, vRect.Left + CurPos + NewPos + GetColumnPosition (ColNumber), vRect.Top + (RowNumber + 1) * WorkBarHeight);
        DrawBar(vRect.Left + CurPos + GetColumnPosition(ColNumber), vRect.Top + RowNumber * WorkBarHeight, vRect.Left + CurPos + NewPos + GetColumnPosition(ColNumber), vRect.Top + (RowNumber + 1) * WorkBarHeight);
      CurPos := CurPos + NewPos;
      DrawBlock := not DrawBlock;
    end;
  end;

  procedure DrawCodewordBitmask(vRect: TRect; RowNumber: Integer; ColNumber: Integer; WorkBarHeight: Integer; Bitmask: DWord);
    function GetColumnPosition(ColNumber: Integer): Integer;
    begin
      Result := ColNumber * StPDF417CellWidth * GetBarWidth;
    end;
  var
    i: Integer;
  begin
    if FHighlight then
    begin
      Canvas.FillRect(
        Rect(vRect.Left + (GetColumnPosition(ColNumber)),
        vRect.Top + RowNumber * WorkBarHeight,
        vRect.Left + 17 * GetBarWidth + GetColumnPosition(ColNumber),
        vRect.Top + (RowNumber + 1) * WorkBarHeight));
      Canvas.Brush.Color := DrawInfo.BarColor; // Color; belal
    end;

    for i := 16 downto 0 do
      if ((BitMask shr i) and $00001) <> 0 then
        //Canvas.Rectangle( vRect.Left + (16 - i) * GetBarWidth + GetColumnPosition (ColNumber), vRect.Top + RowNumber * WorkBarHeight, vRect.Left + (17 - i) * GetBarWidth + GetColumnPosition (ColNumber), vRect.Top + (RowNumber + 1) * WorkBarHeight);
        DrawBar(vRect.Left + (16 - i) * GetBarWidth + GetColumnPosition(ColNumber), vRect.Top + RowNumber * WorkBarHeight, vRect.Left + (17 - i) * GetBarWidth + GetColumnPosition(ColNumber), vRect.Top + (RowNumber + 1) * WorkBarHeight);
  end;

  function DrawLeftRowIndicator(vRect: TRect; RowNumber: Integer; WorkBarHeight: Integer; vRows: Integer; vCols: Integer): Boolean;
  var
    CodeWord: Integer;
    ErrorLevel: Integer;
  begin
    ErrorLevel := GetRealErrorLevel;
    CodeWord := 0;
    if RowNumber mod 3 = 0 then
      CodeWord := ((RowNumber div 3) * 30) + ((vRows - 1) div 3)
    else if RowNumber mod 3 = 1 then
      CodeWord := ((RowNumber div 3) * 30) + ((vRows - 1) mod 3) +
        (3 * ErrorLevel)
    else if RowNumber mod 3 = 2 then
      CodeWord := ((RowNumber div 3) * 30) + (vCols - 1);
    Result := CheckCodeword(CodeWord);
    if Result then
      DrawCodeWordBitmask(vRect, RowNumber, 1, WorkBarHeight, CodewordToBitmask(RowNumber, Codeword));
  end;

  function DrawRightRowIndicator(vRect: TRect; RowNumber: Integer; ColNumber: Integer; WorkBarHeight: Integer; vRows: Integer; vCols: Integer): Boolean;
  var
    Codeword: Integer;
    ErrorLevel: Integer;

  begin
    ErrorLevel := GetRealErrorLevel;
    CodeWord := 0;
    if RowNumber mod 3 = 0 then
      Codeword := ((RowNumber div 3) * 30) + (vCols - 1)
    else if RowNumber mod 3 = 1 then
      Codeword := ((RowNumber div 3) * 30) + ((vRows - 1) div 3)
    else if RowNumber mod 3 = 2 then
      Codeword := ((RowNumber div 3) * 30) + ((vRows - 1) mod 3) +
        (3 * ErrorLevel);
    Result := CheckCodeword(Codeword);
    if Result then
      DrawCodeWordBitmask(vRect, RowNumber, ColNumber, WorkBarHeight, CodewordToBitmask(RowNumber, Codeword));
  end;

  procedure DrawStartPattern(vRect: TRect; RowNumber: Integer; WorkBarHeight: Integer);
  begin
    DrawCodeword(vRect, RowNumber, 0, WorkBarHeight, '81111113');
  end;

  procedure DrawStopPattern(vRect: TRect; RowNumber: Integer; ColNumber: Integer; WorkBarHeight: Integer);
  begin
    if Truncated then
      DrawCodeWord(vRect, RowNumber, ColNumber, WorkBarHeight, '1')
    else
      DrawCodeWord(vRect, RowNumber, ColNumber, WorkBarHeight, '711311121');
  end;

  function CalculateSize(var XSize: Integer; var YSize: Integer): Integer;
  var
    i: Integer;
    NumErrorCodewords: Integer;
    ErrorLevel: Integer;
    j: Integer;

  begin
    { Set the error correction level automatically if needed }
    ErrorLevel := GetRealErrorLevel;
    NumErrorCodewords := Trunc(Power(2, ErrorLevel + 1));
    XSize := NumColumns;
    YSize := NumRows;
    Result := XSize * YSize;

    { Adjust the size if necessary }
    if (NumRows <= 0) or (NumColumns <= 0) then
    begin
      if NumRows > 0 then
      begin
        i := 1;
        while i <= 30 do
        begin
          if i * NumRows - NumErrorCodewords > NumCodewords then
            Break;
          Inc(i);
        end;
        Result := YSize * 30;
        XSize := i;
      end
      else if NumColumns > 0 then
      begin
        i := 3;
        while i <= 90 do
        begin
          if i * NumColumns - NumErrorCodewords > NumCodewords then
            Break;
          Inc(i);
        end;
        YSize := i;
        Result := XSize * 90;
      end
      else
      begin
        i := 1;
        j := 3;
        while (i * j - NumErrorCodewords < NumCodewords) do
        begin
          if j < 90 then
            Inc(j);
          if (i < 30) and (i * j - NumErrorCodewords < NumCodewords) then
            Inc(i);
          if (j >= 90) and (i >= 30) then
            Break;
        end;
        XSize := i;
        YSize := J;
        Result := 900;
      end;
    end;
  end;

  procedure CalculateECC(vNum: Integer; ECCLen: Integer);
  const
    StMods: array[0..64] of array[0..64] of Integer =
    ((0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (917, 27, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (890, 351, 200, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (809, 723, 568, 522, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (566, 155, 460, 919, 427, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (766, 17, 803, 19, 285, 861, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (437, 691, 784, 597, 537, 925, 76, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (379, 428, 653, 646, 284, 436, 308, 237,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (205, 441, 501, 362, 289, 257, 622, 527,
      567, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (612, 266, 691, 818, 841, 826, 244, 64,
      457, 377, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (904, 602, 327, 68, 15, 213, 825, 708,
      565, 45, 462, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (851, 69, 7, 388, 127, 347, 684, 646,
      201, 757, 864, 597, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (692, 394, 184, 204, 678, 592, 322, 583,
      606, 384, 342, 713, 764, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (215, 105, 833, 691, 915, 478, 354, 274,
      286, 241, 187, 154, 677, 669, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (642, 868, 147, 575, 550, 74, 80, 5,
      230, 664, 904, 109, 476, 829, 460, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (65, 176, 42, 295, 428, 442, 116, 295,
      132, 801, 524, 599, 755, 232, 562, 274,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (192, 70, 98, 55, 733, 916, 510, 163,
      437, 843, 61, 259, 650, 430, 298, 115,
      425, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (573, 760, 756, 233, 321, 560, 202, 312,
      297, 120, 739, 275, 855, 37, 624, 315,
      577, 279, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (787, 754, 821, 371, 17, 508, 201, 806,
      177, 506, 407, 491, 249, 923, 181, 75,
      170, 200, 250, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (500, 632, 880, 710, 375, 274, 258, 717,
      176, 802, 109, 736, 540, 64, 45, 152,
      12, 647, 448, 712, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (568, 259, 193, 165, 347, 691, 310, 610,
      624, 693, 763, 716, 422, 553, 681, 425,
      129, 534, 781, 519, 108, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (772, 6, 76, 519, 563, 875, 66, 678,
      578, 716, 927, 296, 633, 244, 155, 928,
      432, 838, 95, 55, 78, 665, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (455, 538, 32, 581, 473, 772, 462, 194,
      251, 503, 631, 1, 630, 247, 843, 101,
      749, 457, 143, 597, 294, 93, 78, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (433, 747, 273, 806, 697, 585, 200, 249,
      628, 555, 713, 54, 608, 322, 54, 135,
      385, 701, 308, 238, 166, 128, 819, 142,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (367, 39, 208, 439, 454, 104, 608, 55,
      916, 912, 314, 375, 760, 141, 169, 287,
      765, 374, 492, 348, 251, 320, 732, 899,
      847, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (169, 764, 847, 131, 858, 325, 454, 441,
      245, 699, 893, 446, 830, 159, 121, 269,
      608, 331, 760, 477, 93, 788, 544, 887,
      284, 443, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (504, 710, 383, 531, 151, 694, 636, 175,
      269, 93, 21, 463, 671, 438, 433, 857,
      610, 560, 165, 531, 100, 357, 688, 114,
      149, 825, 694, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (580, 925, 461, 840, 560, 93, 427, 203,
      563, 99, 586, 201, 557, 339, 277, 321,
      712, 470, 920, 65, 509, 525, 879, 378,
      452, 72, 222, 720, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (808, 318, 478, 42, 706, 500, 264, 14,
      397, 261, 862, 33, 864, 62, 462, 305,
      509, 231, 316, 800, 465, 452, 738, 126,
      239, 9, 845, 241, 656, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (563, 235, 604, 915, 635, 324, 392, 364,
      683, 541, 89, 655, 211, 194, 136, 453,
      104, 12, 390, 487, 484, 794, 549, 471,
      26, 910, 498, 383, 138, 926, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (757, 764, 673, 108, 706, 886, 76, 234,
      695, 196, 66, 270, 8, 252, 612, 825,
      660, 679, 860, 898, 204, 861, 371, 142,
      358, 380, 528, 379, 120, 757, 347, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (410, 63, 330, 685, 390, 231, 133, 803,
      320, 571, 800, 593, 147, 263, 494, 273,
      517, 193, 284, 687, 742, 677, 742, 536,
      321, 640, 586, 176, 525, 922, 575, 361,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (575, 871, 311, 454, 504, 870, 199, 768,
      634, 362, 548, 855, 529, 384, 830, 923,
      222, 85, 841, 59, 518, 590, 358, 110,
      695, 864, 699, 581, 642, 175, 836, 855,
      709, 274, 686, 244, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (5, 10, 156, 729, 684, 324, 60, 264,
      99, 261, 89, 460, 742, 208, 699, 670,
      512, 404, 726, 389, 492, 287, 894, 571,
      41, 203, 353, 256, 243, 784, 385, 555,
      595, 734, 714, 565, 205, 706, 316, 115,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (285, 82, 730, 339, 436, 572, 271, 103,
      758, 231, 560, 31, 213, 272, 267, 569,
      773, 3, 21, 446, 706, 413, 97, 376,
      60, 714, 436, 417, 405, 632, 25, 109,
      876, 470, 915, 157, 840, 764, 64, 678,
      848, 659, 36, 476, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (669, 912, 896, 252, 338, 162, 414, 632,
      626, 252, 869, 185, 444, 82, 920, 783,
      565, 875, 126, 877, 524, 603, 189, 136,
      373, 540, 649, 271, 836, 540, 199, 323,
      888, 486, 92, 849, 162, 701, 178, 926,
      498, 575, 765, 422, 450, 302, 354, 710,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (187, 57, 15, 317, 835, 593, 8, 158,
      95, 145, 37, 659, 576, 386, 884, 913,
      495, 869, 908, 296, 437, 215, 33, 883,
      877, 477, 712, 578, 349, 13, 174, 839,
      914, 107, 260, 40, 532, 210, 395, 905,
      163, 785, 693, 627, 393, 687, 112, 481,
      717, 297, 37, 483, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (163, 726, 626, 653, 414, 537, 467, 579,
      729, 396, 142, 598, 860, 774, 518, 461,
      136, 687, 827, 614, 841, 468, 207, 481,
      649, 910, 497, 686, 186, 235, 845, 863,
      821, 711, 663, 534, 393, 756, 467, 224,
      442, 520, 210, 732, 864, 729, 433, 735,
      70, 184, 278, 97, 492, 17, 2, 338,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (77, 611, 467, 704, 555, 579, 802, 773,
      303, 518, 560, 196, 314, 102, 5, 845,
      248, 125, 836, 923, 88, 630, 886, 619,
      37, 141, 409, 229, 77, 658, 450, 449,
      93, 651, 276, 501, 166, 75, 630, 701,
      388, 72, 830, 166, 187, 131, 711, 577,
      834, 147, 361, 517, 76, 581, 45, 495,
      366, 278, 781, 61, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0),
      (543, 264, 623, 843, 381, 4, 629, 840,
      771, 280, 97, 404, 83, 717, 733, 648,
      502, 488, 201, 651, 158, 605, 352, 517,
      535, 225, 594, 460, 31, 519, 35, 440,
      184, 283, 762, 672, 400, 511, 376, 543,
      822, 858, 609, 430, 172, 462, 476, 723,
      612, 381, 877, 733, 505, 107, 287, 610,
      106, 453, 771, 862, 93, 6, 422, 539, 0));

    StMods128: array[0..127] of Integer =
    (521, 310, 864, 547, 858, 580, 296, 379,
      53, 779, 897, 444, 400, 925, 749, 415,
      822, 93, 217, 208, 928, 244, 583, 620,
      246, 148, 447, 631, 292, 908, 490, 704,
      516, 258, 457, 907, 594, 723, 674, 292,
      272, 96, 684, 432, 686, 606, 860, 569,
      193, 219, 129, 186, 236, 287, 192, 775,
      278, 173, 40, 379, 712, 463, 646, 776,
      171, 491, 297, 763, 156, 732, 95, 270,
      447, 90, 507, 48, 228, 821, 808, 898,
      784, 663, 627, 378, 382, 262, 380, 602,
      754, 336, 89, 614, 87, 432, 670, 616,
      157, 374, 242, 726, 600, 269, 375, 898,
      845, 454, 354, 130, 814, 587, 804, 34,
      211, 330, 539, 297, 827, 865, 37, 517,
      834, 315, 550, 86, 801, 4, 108, 539);

    StMods256: array[0..255] of Integer =
    (524, 894, 75, 766, 882, 857, 74, 204,
      82, 586, 708, 250, 905, 786, 138, 720,
      858, 194, 311, 913, 275, 190, 375, 850,
      438, 733, 194, 280, 201, 280, 828, 757,
      710, 814, 919, 89, 68, 569, 11, 204,
      796, 605, 540, 913, 801, 700, 799, 137,
      439, 418, 592, 668, 353, 859, 370, 694,
      325, 240, 216, 257, 284, 549, 209, 884,
      315, 70, 329, 793, 490, 274, 877, 162,
      749, 812, 684, 461, 334, 376, 849, 521,
      307, 291, 803, 712, 19, 358, 399, 908,
      103, 511, 51, 8, 517, 225, 289, 470,
      637, 731, 66, 255, 917, 269, 463, 830,
      730, 433, 848, 585, 136, 538, 906, 90,
      2, 290, 743, 199, 655, 903, 329, 49,
      802, 580, 355, 588, 188, 462, 10, 134,
      628, 320, 479, 130, 739, 71, 263, 318,
      374, 601, 192, 605, 142, 673, 687, 234,
      722, 384, 177, 752, 607, 640, 455, 193,
      689, 707, 805, 641, 48, 60, 732, 621,
      895, 544, 261, 852, 655, 309, 697, 755,
      756, 60, 231, 773, 434, 421, 726, 528,
      503, 118, 49, 795, 32, 144, 500, 238,
      836, 394, 280, 566, 319, 9, 647, 550,
      73, 914, 342, 126, 32, 681, 331, 792,
      620, 60, 609, 441, 180, 791, 893, 754,
      605, 383, 228, 749, 760, 213, 54, 297,
      134, 54, 834, 299, 922, 191, 910, 532,
      609, 829, 189, 20, 167, 29, 872, 449,
      83, 402, 41, 656, 505, 579, 481, 173,
      404, 251, 688, 95, 497, 555, 642, 543,
      307, 159, 924, 558, 648, 55, 497, 10);

    StMods512: array[0..511] of Integer =
    (352, 77, 373, 504, 35, 599, 428, 207,
      409, 574, 118, 498, 285, 380, 350, 492,
      197, 265, 920, 155, 914, 299, 229, 643,
      294, 871, 306, 88, 87, 193, 352, 781,
      846, 75, 327, 520, 435, 543, 203, 666,
      249, 346, 781, 621, 640, 268, 794, 534,
      539, 781, 408, 390, 644, 102, 476, 499,
      290, 632, 545, 37, 858, 916, 552, 41,
      542, 289, 122, 272, 383, 800, 485, 98,
      752, 472, 761, 107, 784, 860, 658, 741,
      290, 204, 681, 407, 855, 85, 99, 62,
      482, 180, 20, 297, 451, 593, 913, 142,
      808, 684, 287, 536, 561, 76, 653, 899,
      729, 567, 744, 390, 513, 192, 516, 258,
      240, 518, 794, 395, 768, 848, 51, 610,
      384, 168, 190, 826, 328, 596, 786, 303,
      570, 381, 415, 641, 156, 237, 151, 429,
      531, 207, 676, 710, 89, 168, 304, 402,
      40, 708, 575, 162, 864, 229, 65, 861,
      841, 512, 164, 477, 221, 92, 358, 785,
      288, 357, 850, 836, 827, 736, 707, 94,
      8, 494, 114, 521, 2, 499, 851, 543,
      152, 729, 771, 95, 248, 361, 578, 323,
      856, 797, 289, 51, 684, 466, 533, 820,
      669, 45, 902, 452, 167, 342, 244, 173,
      35, 463, 651, 51, 699, 591, 452, 578,
      37, 124, 298, 332, 552, 43, 427, 119,
      662, 777, 475, 850, 764, 364, 578, 911,
      283, 711, 472, 420, 245, 288, 594, 394,
      511, 327, 589, 777, 699, 688, 43, 408,
      842, 383, 721, 521, 560, 644, 714, 559,
      62, 145, 873, 663, 713, 159, 672, 729,
      624, 59, 193, 417, 158, 209, 563, 564,
      343, 693, 109, 608, 563, 365, 181, 772,
      677, 310, 248, 353, 708, 410, 579, 870,
      617, 841, 632, 860, 289, 536, 35, 777,
      618, 586, 424, 833, 77, 597, 346, 269,
      757, 632, 695, 751, 331, 247, 184, 45,
      787, 680, 18, 66, 407, 369, 54, 492,
      228, 613, 830, 922, 437, 519, 644, 905,
      789, 420, 305, 441, 207, 300, 892, 827,
      141, 537, 381, 662, 513, 56, 252, 341,
      242, 797, 838, 837, 720, 224, 307, 631,
      61, 87, 560, 310, 756, 665, 397, 808,
      851, 309, 473, 795, 378, 31, 647, 915,
      459, 806, 590, 731, 425, 216, 548, 249,
      321, 881, 699, 535, 673, 782, 210, 815,
      905, 303, 843, 922, 281, 73, 469, 791,
      660, 162, 498, 308, 155, 422, 907, 817,
      187, 62, 16, 425, 535, 336, 286, 437,
      375, 273, 610, 296, 183, 923, 116, 667,
      751, 353, 62, 366, 691, 379, 687, 842,
      37, 357, 720, 742, 330, 5, 39, 923,
      311, 424, 242, 749, 321, 54, 669, 316,
      342, 299, 534, 105, 667, 488, 640, 672,
      576, 540, 316, 486, 721, 610, 46, 656,
      447, 171, 616, 464, 190, 531, 297, 321,
      762, 752, 533, 175, 134, 14, 381, 433,
      717, 45, 111, 20, 596, 284, 736, 138,
      646, 411, 877, 669, 141, 919, 45, 780,
      407, 164, 332, 899, 165, 726, 600, 325,
      498, 655, 357, 752, 768, 223, 849, 647,
      63, 310, 863, 251, 366, 304, 282, 738,
      675, 410, 389, 244, 31, 121, 303, 263);

  var
    BaseReg: array[0..800] of DWord;
    CoeffReg: array[0..800] of DWord;
    i: Integer;
    j: Integer;
    TInt: Integer;
    Temp: DWord;
    Wrap: DWord;

  begin
    if ECClen < 128 then
      for i := 0 to ECCLen - 1 do
        CoeffReg[i] := StMods[ECClen][i]
    else
    begin
      if ECClen = 128 then
        for i := 0 to ECCLen - 1 do
          CoeffReg[i] := StMods128[i]
      else if ECClen = 256 then
        for i := 0 to ECCLen - 1 do
          CoeffReg[i] := StMods256[i]
      else if ECClen = 512 then
        for i := 0 to ECCLen - 1 do
          CoeffReg[i] := StMods512[i];
    end;

    for i := 0 to ECCLen - 1 do
      BaseReg[i] := 0;

    for i := vNum to vNum + ECCLen - 1 do
      FCodewords[i] := 0;

    for i := 0 to vNum - 1 do
    begin
      wrap := (BaseReg[ECClen - 1] + FCodewords[i]) mod 929;
      for j := ECCLen - 1 downto 1 do
      begin
        temp := (CoeffReg[ECClen - 1 - j] * wrap) mod 929;
        temp := (929 - temp) mod 929;
        BaseReg[j] := (BaseReg[j - 1] + temp) mod 929;
      end;
      temp := (CoeffReg[ECClen - 1] * wrap) mod 929;
      temp := (929 - temp) mod 929;
      BaseReg[0] := temp;
    end;

    for j := 0 to ECCLen - 1 do
      BaseReg[j] := (929 - BaseReg[j]) mod 929;

    for j := 0 to ECCLen - 1 do
    begin
      tint := BaseReg[ECClen - 1 - j];
      FCodewords[vNum + j] := tint;
    end;
  end;

var
  ErrorLevel: Integer;
  NumErrorCodewords: Integer;
  XSize: Integer;
  YSize: Integer;
  //FTotalCodewords: Integer;
  //FUsedCodewords: Integer;
  //FUsedECCCodewords: Integer;
  //FFreeCodewords: Integer;

  i: Integer;
  j: Integer;
  WorkBarHeight: Integer;
  CodewordPos: Integer;
  w: Integer; 
  dr, tr: TRect;
  cw: Integer;
  cw0, dx, th: Integer;
  aAccept: Boolean;

const
  SymbolPadding = 900;

begin
  FHighlight := False;
  Truncated := False;
  FECCLevel := -1;
  NumRows := 0;
  NumColumns := 0;
  Truncated := False;
  

  if PDrawRect <> nil then PDrawRect^ := Rect(0, 0, 0, 0);
  FCodeWords := TextToCodewords(DrawInfo.Code, FNumCodeWords);
  if FCodeWords<>nil then
  begin
    ErrorLevel := GetRealErrorLevel;
    NumErrorCodewords := Trunc(Power(2, ErrorLevel + 1));
    //FTotalCodewords := CalculateSize(XSize, YSize);
    CalculateSize(XSize, YSize);
    cw0 := NumCodewords + (XSize * YSize - NumCodewords - NumErrorCodewords);
    if (cw0>=0) and (NumErrorCodeWords + NumCodeWords <= XSize * YSize) then
    begin
      //FUsedCodewords := NumCodewords;
      //FUsedECCCodewords := NumErrorCodewords;
      //FFreeCodewords := FTotalCodewords - FUsedCodewords;

      FCodewords[0] := cw0;

      CalculateECC(XSize * YSize - NumErrorCodeWords, NumErrorCodewords);

      with DrawInfo do
      begin
        CodewordPos := 1; { The first codeword is always the length }


        if Truncated then
          w := (XSize + 2) * 17 * GetBarWidth + GetBarWidth
        else
          w := (XSize + 4) * 17 * GetBarWidth + GetBarWidth;
        dr := R;
        dr.Right := dr.Left + w;
        if Center then
        begin
          dx := (R.Right-R.Left-w) div 2;
          OffsetRect(dr, dx, 0);
        end;

        if PDrawRect<>nil then PDrawRect^ := dr;

        if not CalcRect then
        begin
          if ShowCode then
          begin
            tr := dr;
            th := Canvas.TextHeight('Yg0');
            tr.Top := tr.Bottom-th;

            DrawString(Canvas, Code, tr, [txtCenter, txtBottom]);
            Dec(dr.Bottom, th);
          end;

          WorkBarHeight := (dr.Bottom - dr.Top) div YSize;
          aAccept := True;
          i := 0;
          Canvas.Brush.Color := clBlack;
          Canvas.Brush.Style := bsSolid;
          Canvas.Pen.Color := clBlack;
          while aAccept and (i < YSize) do
          begin
            if FHighlight then Canvas.Brush.Color := $FFBBFF;
            DrawStartPattern(dr, i, WorkBarHeight);
            if FHighlight then Canvas.Brush.Color := $FFFFBB;
            DrawLeftRowIndicator(dr, i, WorkBarHeight, YSize, XSize);
            j := 0;
            while aAccept and (j < XSize) do
            begin
              if (i = 0) and (j = 0) then
              begin
                if FHighlight then Canvas.Brush.Color := $BBFFFF;
                { Length }
                cw := NumCodewords + (XSize * YSize - NumCodewords - NumErrorCodewords);
                aAccept := CheckCodeword(cw);
                if aAccept then
                  DrawCodeWordBitmask(dr, i, j + 2, WorkBarHeight, CodeWordToBitmask(i, cw))
              end
              else if CodewordPos < NumCodewords then
              begin
                if FHighlight then Canvas.Brush.Color := $BBBBFF;
                { Data }
                cw := FCodewords[CodewordPos];
                aAccept := CheckCodeword(cw);
                if aAccept then
                  DrawCodeWordBitmask(dr, i, j + 2, WorkBarHeight, CodewordToBitmask(i, cw));
                Inc(CodewordPos);
              end
              else if CodewordPos >= XSize * YSize - NumErrorCodeWords then
              begin
                if FHighlight then Canvas.Brush.Color := $FFBBBB;
                { Error Correction Codes }
                cw := FCodewords[CodewordPos];
                aAccept := CheckCodeword(cw);
                if aAccept then
                  DrawCodeWordBitmask(dr, i, j + 2, WorkBarHeight, CodewordToBitmask(i, cw));
                Inc(CodewordPos);
              end
              else
              begin
                if FHighlight then Canvas.Brush.Color := $BBFFBB;
                { Padding }
                cw := SymbolPadding;
                aAccept := CheckCodeword(cw);
                if aAccept then
                  DrawCodewordBitmask(dr, i, j + 2, WorkBarHeight, CodewordToBitmask(i, cw));
                Inc(CodewordPos);
              end;
              j := j + 1;
            end; //while aAccept and (j < XSize) do
            if aAccept then
            begin
              if FHighlight then
                Canvas.Brush.Color := $BBDDFF;
              if Truncated then
                DrawStopPattern(dr, i, XSize + 2, WorkBarHeight)
              else
              begin
                DrawRightRowIndicator(dr, i, XSize + 2, WorkBarHeight, YSize, XSize);
                if FHighlight then
                  Canvas.Brush.Color := $DDAAFF;
                DrawStopPattern(dr, i, XSize + 3, WorkBarHeight);
              end;
            end;
            i := i + 1;
          end; //while i
        end; //if not calc rect
      end;
    end;//if (cw0>=0) and (NumErrorCodeWords + NumCodeWords <= XSize * YSize) then
  end;
end;

//*********************************************************

//*************************************

procedure DrawStickerFrame(Canvas: TCanvas; R: TRect);
begin
  with Canvas do
  begin
    InflateRect(R, -1, -1);
    MoveTo(R.Left, R.Top);
    LineTo(R.Right, R.Top);
    LineTo(R.Right, R.Bottom);
    LineTo(R.Left, R.Bottom);
    LineTo(R.Left, R.Top);
    InflateRect(R, 1, 1);
  end;
end;

{*** TBarcodeInfo ***}

procedure TBarcodeInfo.Add(ModuleCount: Integer; BarKind: TStBarKindSet);
var
  Bar: TBarData;
begin
  Bar := TBarData.Create;
  Bar.Modules := ModuleCount;
  Bar.Kind := BarKind;
  FBars.Add(Bar);
end;

procedure TBarcodeInfo.Clear;
begin
  FBars.Clear;
end;

constructor TBarcodeInfo.Create;
begin
  inherited Create;
  FBars := TObjectList.Create(True);
end;

destructor TBarcodeInfo.Destroy;
begin
  FBars.Free;
  inherited Destroy;
end;

function TBarcodeInfo.GetBars(Index: Integer): TBarData;
begin
  Result := TBarData(FBars[Index]);
end;

function TBarcodeInfo.GetCount: Integer;
begin
  Result := FBars.Count;
end;

const
  {left and right codes for UPC_A}
  UPC_A_LeftHand: array[0..9] of string[8] =
  ('0001101', {0}
    '0011001', {1}
    '0010011', {2}
    '0111101', {3}
    '0100011', {4}
    '0110001', {5}
    '0101111', {6}
    '0111011', {7}
    '0110111', {8}
    '0001011' {9});

  UPC_A_RightHand: array[0..9] of string[8] =
  ('1110010', {0}
    '1100110', {1}
    '1101100', {2}
    '1000010', {3}
    '1011100', {4}
    '1001110', {5}
    '1010000', {6}
    '1000100', {7}
    '1001000', {8}
    '1110100' {9});

const
  UPC_E_OddParity: array[0..9] of string[8] =
  ('0001101', {0}
    '0011001', {1}
    '0010011', {2}
    '0111101', {3}
    '0100011', {4}
    '0110001', {5}
    '0101111', {6}
    '0111011', {7}
    '0110111', {8}
    '0001011' {9});

  UPC_E_EvenParity: array[0..9] of string[8] =
  ('0100111', {0}
    '0110011', {1}
    '0011011', {2}
    '0100001', {3}
    '0011101', {4}
    '0111001', {5}
    '0000101', {6}
    '0010001', {7}
    '0001001', {8}
    '0010111' {9});

const
  EAN_LeftHandA: array[0..9] of string[8] =
  ('0001101', {0}
    '0011001', {1}
    '0010011', {2}
    '0111101', {3}
    '0100011', {4}
    '0110001', {5}
    '0101111', {6}
    '0111011', {7}
    '0110111', {8}
    '0001011' {9});

  EAN_LeftHandB: array[0..9] of string[8] =
  ('0100111', {0}
    '0110011', {1}
    '0011011', {2}
    '0100001', {3}
    '0011101', {4}
    '0111001', {5}
    '0000101', {6}
    '0010001', {7}
    '0001001', {8}
    '0010111' {9});

const
  Interleaved_2of5: array[0..9] of string[5] =
  ('00110', {0}
    '10001', {1}
    '01001', {2}
    '11000', {3}
    '00101', {4}
    '10100', {5}
    '01100', {6}
    '00011', {7}
    '10010', {8}
    '01010' {9});

const
  Codabar: array[0..19] of string[7] =
  {BSBSBSB}{bar-space-bar-space-bar...}
  ('0000011', {0}
    '0000110', {1}
    '0001001', {2}
    '1100000', {3}
    '0010010', {4}
    '1000010', {5}
    '0100001', {6}
    '0100100', {7}
    '0110000', {8}
    '1001000', {9}
    '0001100', {-}
    '0011000', { $}
    '1000101', {:}
    '1010001', {/}
    '1010100', {.}
    '0010101', {+}
    '0011010', {A}
    '0101001', {B}
    '0001011', {C}
    '0001110' {D});

const
  Code11: array[0..11] of string[5] =
  {BSBSB}{bar-space-bar-space-bar...}{0-narrow, 1-wide}
  ('00001', {0}
    '10001', {1}
    '01001', {2}
    '11000', {3}
    '00101', {4}
    '10100', {5}
    '01100', {6}
    '00011', {7}
    '10010', {8}
    '10000', {9}
    '00100', {-}
    '00110'); {stop character}

const
  Code39: array[0..43] of string[9] =
  {BSBSBSBSB}{bar-space-bar-space-bar...}{0-narrow, 1-wide}
  ('000110100', {0}
    '100100001', {1}
    '001100001', {2}
    '101100000', {3}
    '000110001', {4}
    '100110000', {5}
    '001110000', {6}
    '000100101', {7}
    '100100100', {8}
    '001100100', {9}
    '100001001', {A}
    '001001001', {B}
    '101001000', {C}
    '000011001', {D}
    '100011000', {E}
    '001011000', {F}
    '000001101', {G}
    '100001100', {H}
    '001001100', {I}
    '000011100', {J}
    '100000011', {K}
    '001000011', {L}
    '101000010', {M}
    '000010011', {N}
    '100010010', {O}
    '001010010', {P}
    '000000111', {Q}
    '100000110', {R}
    '001000110', {S}
    '000010110', {T}
    '110000001', {U}
    '011000001', {V}
    '111000000', {W}
    '010010001', {X}
    '110010000', {Y}
    '011010000', {Z}
    '010000101', {-}
    '110000100', {.}
    '011000100', {SPACE}
    '010101000', { $}
    '010100010', {/}
    '010001010', {+}
    '000101010', {%}
    '010010100'); {*}

const
  Code93: array[0..46] of string[9] =
  {BSBSBS}{bar-space-bar-space-bar...}{0-narrow, 1-wide}
  ('131112', {0}
    '111213', {1}
    '111312', {2}
    '111411', {3}
    '121113', {4}
    '121212', {5}
    '121311', {6}
    '111114', {7}
    '131211', {8}
    '141111', {9}
    '211113', {A}
    '211212', {B}
    '211311', {C}
    '221112', {D}
    '221211', {E}
    '231111', {F}
    '112113', {G}
    '112212', {H}
    '112311', {I}
    '122112', {J}
    '132111', {K}
    '111123', {L}
    '111222', {M}
    '111321', {N}
    '121122', {O}
    '131121', {P}
    '212112', {Q}
    '212211', {R}
    '211122', {S}
    '211221', {T}
    '221121', {U}
    '222111', {V}
    '112122', {W}
    '112221', {X}
    '122121', {Y}
    '123111', {Z}
    '121131', {-}
    '311112', {.}
    '311211', {SPACE}
    '321111', { $}
    '112131', {/}
    '113121', {+}
    '211131', {%}
    '121221', {($)}
    '312111', {(%)}
    '311121', {(/)}
    '122211'); {(+)}

  Code93Map: array[#0..#127] of string[2] =
  {Circle Code}{ASCII Code 93 }
  ('%U', {NL     (%)U   }
    '$A', {SH     ($)A   }
    '$B', {SX     ($)B   }
    '$C', {EX     ($)C   }
    '$D', {ET     ($)D   }
    '$E', {EQ     ($)E   }
    '$F', {AK     ($)F   }
    '$G', {BL     ($)G   }
    '$H', {BS     ($)H   }
    '$I', {HT     ($)I   }
    '$J', {LF     ($)J   }
    '$K', {VT     ($)K   }
    '$L', {FF     ($)L   }
    '$M', {CR     ($)M   }
    '$N', {SO     ($)N   }
    '$O', {SI     ($)O   }
    '$P', {DL     ($)P   }
    '$Q', {D1     ($)Q   }
    '$R', {D2     ($)R   }
    '$S', {D3     ($)S   }
    '$T', {D4     ($)T   }
    '$U', {NK     ($)U   }
    '$V', {SY     ($)V   }
    '$W', {EB     ($)W   }
    '$X', {CN     ($)X   }
    '$Y', {EM     ($)Y   }
    '$Z', {SB     ($)Z   }
    '%A', {EC     (%)A   }
    '%B', {FS     (%)B   }
    '%C', {GS     (%)C   }
    '%D', {RS     (%)D   }
    '%E', {US     (%)E   }
    ' ', {Space   Space }
    '/A', {!      (/)A   }
    '/B', {"      (/)B   }
    '/C', {#      (/)C   }
    '$', { $   (/)D or $}
    '%', {%    (/)E or %}
    '/F', {&      (/)F   }
    '/G', {'      (/)G   }
    '/H', {(      (/)H   }
    '/I', {)      (/)I   }
    '/J', {*      (/)J   }
    ' +', {+    (/)K or +}
    '/L', {,      (/)L   }
    '-', {-    (/)M or -}
    '.', {.    (/)N or .}
    '/', {/    (/)O or /}
    '0', {0    (/)P or 0}
    '1', {1    (/)Q or 1}
    '2', {2    (/)R or 2}
    '3', {3    (/)S or 3}
    '4', {4    (/)T or 4}
    '5', {5    (/)U or 5}
    '6', {6    (/)V or 6}
    '7', {7    (/)W or 7}
    '8', {8    (/)X or 8}
    '9', {9    (/)Y or 9}
    '/Z', {:      (/)Z   }
    '%F', {;      (%)F   }
    '%G', {<      (%)G   }
    '%H', {=      (%)H   }
    '%I', {>      (%)I   }
    '%J', {?      (%)J   }
    '%V', {       (%)V   }
    'A', {A   	    A    }
    'B', {B	    B    }
    'C', {C	    C    }
    'D', {D	    D    }
    'E', {E	    E    }
    'F', {F	    F    }
    'G', {G	    G    }
    'H', {H	    H    }
    'I', {I	    I    }
    'J', {J	    J    }
    'K', {K	    K    }
    'L', {L	    L    }
    'M', {M	    M    }
    'N', {N	    N    }
    'O', {O	    O    }
    'P', {P	    P    }
    'Q', {Q	    Q    }
    'R', {R	    R    }
    'S', {S	    S    }
    'T', {T        T    }
    'U', {U	    U    }
    'V', {V	    V    }
    'W', {W	    W    }
    'X', {X	    X    }
    'Y', {Y	    Y    }
    'Z', {Z	    Z    }
    '%K', {[  	   (%)K  }
    '%L', {\  	   (%)L  }
    '%M', {]  	   (%)M  }
    '%N', {^  	   (%)N  }
    '%O', {_  	   (%)O  }
    '%W', {`       (%)W  }
    '+A', {a  	   (+)A  }
    '+B', {b  	   (+)B  }
    '+C', {c  	   (+)C  }
    '+D', {d  	   (+)D  }
    '+E', {e  	   (+)E  }
    '+F', {f  	   (+)F  }
    '+G', {g  	   (+)G  }
    '+H', {h  	   (+)H  }
    '+I', {i  	   (+)I  }
    '+J', {j  	   (+)J  }
    '+K', {k  	   (+)K  }
    '+L', {l  	   (+)L  }
    '+M', {m  	   (+)M  }
    '+N', {n  	   (+)N  }
    '+O', {o  	   (+)O  }
    '+P', {p  	   (+)P  }
    '+Q', {q  	   (+)Q  }
    '+R', {r  	   (+)R  }
    '+S', {s  	   (+)S  }
    '+T', {t  	   (+)T  }
    '+U', {u  	   (+)U  }
    '+V', {v  	   (+)V  }
    '+W', {w  	   (+)W  }
    '+X', {x  	   (+)X  }
    '+Y', {y  	   (+)Y  }
    '+Z', {z  	   (+)Z  }
    '%P', {{  	   (%)P  }
    '%Q', {|  	   (%)Q  }
    '%R', {} {	   (%)R  }
    '%S', {~  	   (%)S  }
    '%T'); { DEL    (%)T  }

const
  Code128: array[0..106] of string[7] =
  {BSBSBS}{Value  CodeA  CodeB   CodeC}
  ('212222', {0	SPACE	SPACE	00}
    '222122', {1	!	!	01}
    '222221', {2	"	"	02}
    '121223', {3	#	#	03}
    '121322', {4	$	$	04}
    '131222', {5	%	%	05}
    '122213', {6	&	&	06}
    '122312', {7	'	'	07}
    '132212', {8	(	(	08}
    '221213', {9	)	)	09}
    '221312', {10	* 	*	10}
    '231212', {11	+	+	11}
    '112232', {12	,	,	12}
    '122132', {13	-	-	13}
    '122231', {14	.	.	14}
    '113222', {15	/	/	15}
    '123122', {16	0	0	16}
    '123221', {17	1	1	17}
    '223211', {18	2	2	18}
    '221132', {19	3	3	19}
    '221231', {20	4	4	20}
    '213212', {21	5	5	21}
    '223112', {22	6	6	22}
    '312131', {23	7	7	23}
    '311222', {24	8	8	24}
    '321122', {25	9	9	25}
    '321221', {26	:	:	26}
    '312212', {27	;	;	27}
    '322112', {28	<	<	28}
    '322211', {29	= 	= 	29}
    '212123', {30	>	>	30}
    '212321', {31	?	?	31}
    '232121', {32	@	@	32}
    '111323', {33	A	A	33}
    '131123', {34	B	B	34}
    '131321', {35	C	C	35}
    '112313', {36	D	D	36}
    '132113', {37	E	E	37}
    '132311', {38	F	F	38}
    '211313', {39	G	G	39}
    '231113', {40	H	H	40}
    '231311', {41	I	I	41}
    '112133', {42	J	J	42}
    '112331', {43	K	K	43}
    '132131', {44	L	L	44}
    '113123', {45	M	M	45}
    '113321', {46	N	N	46}
    '133121', {47	O	O	47}
    '313121', {48	P	P	48}
    '211331', {49	Q	Q	49}
    '231131', {50	R	R	50}
    '213113', {51	S	S	51}
    '213311', {52	T	T	52}
    '213131', {53	U	U	53}
    '311123', {54	V	V	54}
    '311321', {55	W	W	55}
    '331121', {56	X	X	56}
    '312113', {57	Y	Y	57}
    '312311', {58	Z	Z	58}
    '332111', {59	[	[	59}
    '314111', {60	\	\	60}
    '221411', {61	]	]	61}
    '431111', {62	^	^	62}
    '111224', {63	_ 	_ 	63}
    '111422', {64	NU	`	64}
    '121124', {65	SH	a	65}
    '121421', {66	SX	b	66}
    '141122', {67	EX	c	67}
    '141221', {68	ET	d	68}
    '112214', {69	EQ	e	69}
    '112412', {70	AK	f	70}
    '122114', {71	BL	g	71}
    '122411', {72	BS	h	72}
    '142112', {73	HT	i	73}
    '142211', {74	LF	j	74}
    '241211', {75	VT	k	75}
    '221114', {76	FF	l	76}
    '413111', {77	CR	m	77}
    '241112', {78	SO	n	78}
    '134111', {79	SI	o	79}
    '111242', {80	DL	p	80}
    '121142', {81	D1	q	81}
    '121241', {82	D2	r	82}
    '114212', {83	D3	s	83}
    '124112', {84	D4	t	84}
    '124211', {85	NK	u	85}
    '411212', {86	SY	v	86}
    '421112', {87	EB	w	87}
    '421211', {88	CN	x	88}
    '212141', {89	EM	y	89}
    '214121', {90	SB	z	90}
    '412121', (*91	EC	{	91*)
    '111143', {92	FS		92}
    '111341', (*93	GS	}	93*)
    '131141', {94	RS	~	94}
    '114113', {95	US	DEL	95}
    '114311', {96	FNC 3	FNC 3	96} {use #132}
    '411113', {97	FNC 2	FNC 2	97} {use #131}
    '411311', {98	SHIFT	SHIFT	98} {use #130}
    '113141', {99	CODE C	CODE C	99} {use #135}
    '114131', {100	CODE B	FNC 4	CODE B} {use #134}
    '311141', {101	FNC 4	CODE A	CODE A} {use #133}
    '411131', {102	FNC 1	FNC 1	FNC 1 } {use #130}
    '211412', {103	CODE A} {use #136}
    '211214', {104	CODE B} {use #137}
    '211232', {105	CODE C} {use #138}
    '2331112'); {106    STOP} {use #139}

  {*** helper routines ***}

procedure CalcBarCode(DrawInfo: TBarcodeDrawInfo; var Digits: TStDigitArray; var Info: TBarcodeInfo);
var
  I, J, X: Integer;
  CheckC: Integer;
  CheckK: Integer;
  CSP: string;
  C: string;
  C1, C2: string;

var
  DigitCount: Integer;

  procedure AddCode(const S: string; AKind: TStBarKindSet);
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do
      if S[I] = '0' then
        Info.Add(1, AKind - [bkBar, bkThreeQuarterBar, bkHalfBar] + [bkSpace])
      else
        Info.Add(StrToIntDef(S[I], 0), AKind);
  end;

  procedure AddECode(const Parity: string);
  var
    I: Integer;
  begin
    for I := 1 to Length(Parity) do
    begin
      if Parity[I] = 'E' then
        AddCode(UPC_E_EvenParity[Digits[I]], [bkBar])
      else
        AddCode(UPC_E_OddParity[Digits[I]], [bkBar]);
    end;
  end;

  procedure AddSupCode(const Parity: string);
  var
    I: Integer;
  begin
    for I := 1 to Length(Parity) do
    begin
      if Parity[I] = 'E' then
        AddCode(UPC_E_EvenParity[Digits[I]], [bkThreeQuarterBar, bkSupplement])
      else
        AddCode(UPC_E_OddParity[Digits[I]], [bkThreeQuarterBar, bkSupplement]);
      if I < Length(Parity) then
        AddCode('01', [bkThreeQuarterBar, bkSupplement]);
    end;
  end;

  procedure AddCodeModules(const S: string);
  var
    K: Integer;
  begin
    for K := 1 to Length(S) do
    begin
      if Odd(K) then
        Info.Add(StrToIntDef(S[K], 0), [bkBar])
      else
        Info.Add(StrToIntDef(S[K], 0), [bkSpace]);
    end;
  end;

  procedure AddCodeWideNarrow(const S: string);
  var
    K: Integer;
  begin
    with DrawInfo do
    begin
      for K := 1 to Length(S) do
      begin
        case S[K] of
          '0':
            if Odd(K) then
              Info.Add(1, [bkBar])
            else
              Info.Add(1, [bkSpace]);
          '1':
            if Odd(K) then
              Info.Add(BarNarrowToWideRatio, [bkBar])
            else
              Info.Add(BarNarrowToWideRatio, [bkSpace]);
        end;
      end;
    end;
  end;

begin
  with DrawInfo do
  begin
    Info.Clear;
    if Code = '' then
      Exit;

    {get copy of code}
    C := Code;

    {get digits}
    case BarcodeType of
      bcUPC_A, bcUPC_E, bcEAN_8, bcEAN_13, bcCodabar, bcCode11, bcCode93:
        begin
          DigitCount := GetDigits(C, Digits, BarcodeType);
        end;
      bcInterleaved2of5:
        begin
          {adjust odd length code}
          if AddCheckChar then
          begin
            if not Odd(Length(C)) then
              C := '0' + C;
          end
          else
          begin
            if Odd(Length(C)) then
              C := '0' + C;
          end;
          DigitCount := GetDigits(C, Digits, BarcodeType);
        end;
      bcCode39:
        begin
          {add guard characters}
          if C[1] <> '*' then
            C := '*' + C;
          if C[Length(C)] <> '*' then
            C := C + '*';
          DigitCount := GetDigits(C, Digits, BarcodeType);
        end;
      bcCode128:
        begin
          {add start code}
          if not (C[1] in [#136, #137, #138]) then
            case Code128Subset of
              csCodeA: C := #136 + C;
              csCodeB: C := #137 + C;
              csCodeC: C := #138 + C;
            end;
          DigitCount := GetDigits(C, Digits, BarcodeType);
        end;
    end;

    case BarcodeType of
      bcUPC_A:
        begin
          {get check digit}
          if Length(C) = 11 then
            GetCheckCharacters(C, CheckC, CheckK, BarcodeType, Digits, DigitCount)
          else
            CheckC := Digits[12];

          {encode left hand guard bars}
          AddCode('101', [bkGuard, bkBar]);

          {first six characters as left hand characters}
          for I := 1 to 6 do
            AddCode(UPC_A_LeftHand[Digits[I]], [bkBar]);

          {center guard pattern}
          AddCode('01010', [bkGuard, bkBar]);

          {last five data characters as right hand characters}
          for I := 7 to 11 do
            AddCode(UPC_A_RightHand[Digits[I]], [bkBar]);

          {check character}
          AddCode(UPC_A_RightHand[CheckC], [bkBar]);

          {encode right hand guard bars}
          AddCode('101', [bkGuard, bkBar]);
        end;
      bcUPC_E:
        begin
          {encode left hand guard bars, 101}
          AddCode('101', [bkGuard, bkBar]);
          GetCheckCharacters(C, CheckC, CheckK, BarcodeType, Digits, DigitCount);
          case CheckC of
            0: AddECode('EEEOOO');
            1: AddECode('EEOEOO');
            2: AddECode('EEOOEO');
            3: AddECode('EEOOOE');
            4: AddECode('EOEEOO');
            5: AddECode('EOOEEO');
            6: AddECode('EOOOEE');
            7: AddECode('EOEOEO');
            8: AddECode('EOEOOE');
            9: AddECode('EOOEOE');
          end;
          {encode right hand guard bars}
          AddCode('010101', [bkGuard, bkBar]);
        end;
      bcEAN_8:
        begin
          {get check digit}
          if Length(C) = 7 then
            GetCheckCharacters(C, CheckC, CheckK, BarcodeType, Digits, DigitCount)
          else
            CheckC := Digits[8];

          {encode left hand guard bars}
          AddCode('101', [bkGuard, bkBar]);
          {two flag two data characters, encoded as left hand A characters}
          for I := 1 to 4 do
            AddCode(EAN_LeftHandA[Digits[I]], [bkBar]);
          {encode center guard bars}
          AddCode('01010', [bkGuard, bkBar]);
          {last three data characters, encoded as right hand characters}
          for I := 5 to 7 do
            AddCode(UPC_A_RightHand[Digits[I]], [bkBar]);
          {check character}
          AddCode(UPC_A_RightHand[CheckC], [bkBar]);
          {encode right hand guard bars}
          AddCode('101', [bkGuard, bkBar]);
        end;
      bcEAN_13:
        begin
          {get check digit}
          if Length(C) = 12 then
            GetCheckCharacters(C, CheckC, CheckK, BarcodeType, Digits, DigitCount)
          else
            CheckC := Digits[13];

          {determine which left hand table to use based on first flag character}
          {EAN refers to this as the 13th digit - counting from the right}
          case Digits[1] of
            { 12345}
            0: CSP := 'AAAAAA';
            1: CSP := 'AABABB';
            2: CSP := 'AABBAB';
            3: CSP := 'AABBBA';
            4: CSP := 'ABAABB';
            5: CSP := 'ABBAAB';
            6: CSP := 'ABBBAA';
            7: CSP := 'ABABAB';
            8: CSP := 'ABABBA';
            9: CSP := 'ABBABA';
          end;
          {encode left hand guard bars}
          AddCode('101', [bkGuard, bkBar]);
          {start with second flag character and next five data characters}
          for I := 2 to 7 do
            if CSP[I - 1] = 'A' then
              AddCode(EAN_LeftHandA[Digits[I]], [bkBar])
            else
              AddCode(EAN_LeftHandB[Digits[I]], [bkBar]);
          {encode center guard bars}
          AddCode('01010', [bkGuard, bkBar]);
          {encode last five data characters}
          for I := 8 to 12 do
            AddCode(UPC_A_RightHand[Digits[I]], [bkBar]);
          {check character}
          AddCode(UPC_A_RightHand[CheckC], [bkBar]);
          {encode right hand guard bars}
          AddCode('101', [bkGuard, bkBar]);
        end;
      bcInterleaved2of5:
        begin
          {add check character}
          if AddCheckChar then
          begin
            {get check digit}
            GetCheckCharacters(C, CheckC, CheckK, BarcodeType, Digits, DigitCount);
            Inc(DigitCount);
            Digits[DigitCount] := CheckC;
          end;

          {encode left guard pattern}
          Info.Add(1, [bkGuard, bkBar]);
          Info.Add(1, [bkGuard, bkSpace]);
          Info.Add(1, [bkGuard, bkBar]);
          Info.Add(1, [bkGuard, bkSpace]);

          I := 1;
          while I < DigitCount do
          begin
            {take two characters at a time - odd as bars, even as spaces}
            C1 := Interleaved_2of5[Digits[I]];
            C2 := Interleaved_2of5[Digits[I + 1]];
            {interleave data}
            for J := 1 to 5 do
            begin
              if C1[J] = '1' then
                Info.Add(BarNarrowToWideRatio, [bkBar]) {wide bar}
              else
                Info.Add(1, [bkBar]); {narrow bar}
              if C2[J] = '1' then
                Info.Add(BarNarrowToWideRatio, [bkSpace]) {wide space}
              else
                Info.Add(1, [bkSpace]); {narrow space}
            end;
            Inc(I, 2);
          end;

          {encode right guard pattern}
          Info.Add(BarNarrowToWideRatio, [bkGuard, bkBar]); {double-width bar}
          Info.Add(1, [bkGuard, bkSpace]);
          Info.Add(1, [bkGuard, bkBar]);
        end;
      bcCodabar:
        begin
          for I := 1 to DigitCount do
          begin
            AddCodeWideNarrow(Codabar[Digits[I]]);
            if I < DigitCount then
              Info.Add(1, [bkSpace]);
          end;
        end;
      bcCode11:
        begin
          AddCodeWideNarrow(Code11[11]); {start}
          Info.Add(1, [bkSpace]);
          {add check characters}
          if AddCheckChar then
          begin
            {get check digits}
            GetCheckCharacters(C, CheckC, CheckK, BarcodeType, Digits, DigitCount);
            Inc(DigitCount);
            Digits[DigitCount] := CheckC;
            Inc(DigitCount);
            Digits[DigitCount] := CheckK;
          end;

          for I := 1 to DigitCount do
          begin
            AddCodeWideNarrow(Code11[Digits[I]]);
            Info.Add(1, [bkSpace]);
          end;
          AddCodeWideNarrow(Code11[11]); {stop}
        end;
      bcCode39:
        begin
          for I := 1 to DigitCount do
          begin
            C1 := Code39[Digits[I]];
            for J := 1 to Length(C1) do
            begin
              case C1[J] of
                '0':
                  if Odd(J) then
                    Info.Add(1, [bkBar])
                  else
                    Info.Add(1, [bkSpace]);
                '1':
                  if Odd(J) then
                    Info.Add(2, [bkBar])
                  else
                    Info.Add(2, [bkSpace]);
              end;
            end;
            Info.Add(1, [bkSpace]);
          end;
        end;
      bcCode93:
        begin
          ;
          {start character}
          AddCodeModules('111141');
          {add check characters}
          if AddCheckChar then
          begin
            {get check digits}
            GetCheckCharacters(C, CheckC, CheckK, BarcodeType, Digits, DigitCount);
            Inc(DigitCount);
            Digits[DigitCount] := CheckC;
            Inc(DigitCount);
            Digits[DigitCount] := CheckK;
          end;
          for I := 1 to DigitCount do
            AddCodeModules(Code93[Digits[I]]);
          {stop character}
          AddCodeModules('1111411');
        end;
      bcCode128:
        begin
          {add check character}
          if AddCheckChar then
          begin
            GetCheckCharacters(C, CheckC, CheckK, BarcodeType, Digits, DigitCount);
            Inc(DigitCount);
            Digits[DigitCount] := CheckC;
          end;
          {add stop code}
          Inc(DigitCount);
          Digits[DigitCount] := 106;
          for I := 1 to DigitCount do
            AddCodeModules(Code128[Digits[I]]);
        end;
    end;

    if BarcodeType in [bcUPC_A, bcUPC_E, bcEAN_8, bcEAN_13] then
    begin
      {add supplemental encodings if requested}
      if Length(SupplementalCode) in [2, 5] then
      begin
        {get digits}
        DigitCount := GetDigits(SupplementalCode, Digits, BarcodeType);
        {7 spaces after primary code - 0000000}
        AddCode('0000000', [bkThreeQuarterBar, bkBlankSpace]);
        {encode left hand guard bars, 1011}
        AddCode('1011', [bkThreeQuarterBar, bkSupplement]);

        if DigitCount = 2 then
        begin
          {two digit supplement}
          {determine parity table to use for each of the two characters}
          X := Digits[1] * 10 + Digits[2];
          case X mod 4 of
            0: AddSupCode('OO');
            1: AddSupCode('OE');
            2: AddSupCode('EO');
            3: AddSupCode('EE');
          end;
        end
        else
        begin
          {five digit supplement}
          {determine the parity pattern to use for each of the five}
          X := ((Digits[1] + Digits[3] + Digits[5]) * 3 + (Digits[2] + Digits[4]) * 9) mod 10;
          case X of
            0: AddSupCode('EEOOO');
            1: AddSupCode('EOEOO');
            2: AddSupCode('EOOEO');
            3: AddSupCode('EOOOE');
            4: AddSupCode('OEEOO');
            5: AddSupCode('OOEEO');
            6: AddSupCode('OOOEE');
            7: AddSupCode('OEOEO');
            8: AddSupCode('OEOOE');
            9: AddSupCode('OOEOE');
          end;
        end;
      end;
    end;
  end;
end;

function GetDigits(Characters: string; var Digits: TStDigitArray; BarcodeType: TBarcodeType; Code128Subset: TCode128CodeSubset): Integer;

const //belal
  ExtendedSyntax: Boolean = False;

  procedure GetACode128CDigit(c: Char; var Index: Integer;
    var bcDigitPos: Integer);
  var
    J: Integer;

  begin
    case (c) of
      #130: Digits[bcDigitPos + 1] := 98; {rest are manufactured characters}
      #131: Digits[bcDigitPos + 1] := 97;
      #132: Digits[bcDigitPos + 1] := 96;
      #133: Digits[bcDigitPos + 1] := 98;
      #134: Digits[bcDigitPos + 1] := 100;
      #135: Digits[bcDigitPos + 1] := 99;
      #136: Digits[bcDigitPos + 1] := 103;
      #137: Digits[bcDigitPos + 1] := 104;
      #138: Digits[bcDigitPos + 1] := 105;
      #139: Digits[bcDigitPos + 1] := 106;
    else
      try
        J := StrToIntDef(Copy(Characters, Index, 2), 0);
        Digits[bcDigitPos + 1] := J;
        Inc(Index);
      except
        RaiseError(EStBarCodeError, 'stscInvalidCharacter');
      end;
    end;
    Inc(Index);
    Inc(bcDigitPos);
  end;

  procedure GetACode128ABDigit(c: Char; var Index: Integer;
    var bcDigitPos: Integer);
  begin
    case c of
      ' ': Digits[bcDigitPos + 1] := 0;
      '!': Digits[bcDigitPos + 1] := 1;
      '"': Digits[bcDigitPos + 1] := 2;
      '#': Digits[bcDigitPos + 1] := 3;
      '$': Digits[bcDigitPos + 1] := 4;
      '%': Digits[bcDigitPos + 1] := 5;
      '&': Digits[bcDigitPos + 1] := 6;
      '''': Digits[bcDigitPos + 1] := 7;
      '(': Digits[bcDigitPos + 1] := 8;
      ')': Digits[bcDigitPos + 1] := 9;
      '*': Digits[bcDigitPos + 1] := 10;
      '+': Digits[bcDigitPos + 1] := 11;
      ',': Digits[bcDigitPos + 1] := 12;
      '-': Digits[bcDigitPos + 1] := 13;
      '.': Digits[bcDigitPos + 1] := 14;
      '/': Digits[bcDigitPos + 1] := 15;
      '0'..'9': Digits[bcDigitPos + 1] := 16 + Ord(c) - Ord('0');
      ':': Digits[bcDigitPos + 1] := 26;
      ';': Digits[bcDigitPos + 1] := 27;
      '<': Digits[bcDigitPos + 1] := 28;
      '=': Digits[bcDigitPos + 1] := 29;
      '>': Digits[bcDigitPos + 1] := 30;
      '?': Digits[bcDigitPos + 1] := 31;
      '@': Digits[bcDigitPos + 1] := 32;
      'A'..'Z': Digits[bcDigitPos + 1] := 33 + Ord(c) - Ord('A');
      '[': Digits[bcDigitPos + 1] := 59;
      '\': Digits[bcDigitPos + 1] := 60;
      ']': Digits[bcDigitPos + 1] := 61;
      '^': Digits[bcDigitPos + 1] := 62;
      '_': Digits[bcDigitPos + 1] := 63;
      #0, #31: Digits[bcDigitPos + 1] := 64 + Ord(c); {control characters}
      '`': Digits[bcDigitPos + 1] := 64;
      'a'..'z': Digits[bcDigitPos + 1] := 65 + Ord(c) - Ord('a');
      '{': Digits[bcDigitPos + 1] := 91;
      '|': Digits[bcDigitPos + 1] := 92;
      '}': Digits[bcDigitPos + 1] := 93;
      '~': Digits[bcDigitPos + 1] := 94;
      #130: Digits[bcDigitPos + 1] := 98; {rest are manufactured characters}
      #131: Digits[bcDigitPos + 1] := 97;
      #132: Digits[bcDigitPos + 1] := 96;
      #133: Digits[bcDigitPos + 1] := 98;
      #134: Digits[bcDigitPos + 1] := 100;
      #135: Digits[bcDigitPos + 1] := 99;
      #136: Digits[bcDigitPos + 1] := 103;
      #137: Digits[bcDigitPos + 1] := 104;
      #138: Digits[bcDigitPos + 1] := 105;
      #139: Digits[bcDigitPos + 1] := 106;
    else
      RaiseError(EStBarCodeError, 'stscInvalidCharacter');
    end;
    Inc(Index);
    Inc(bcDigitPos);
  end;

  function CountCode128Digits(Index: Integer): Integer;
  begin
    Result := 0;
    while (Index <= Length(Characters)) and
      (Characters[Index] >= '0') and (Characters[Index] <= '9') do
    begin
      Inc(Result);
      Inc(Index);
    end;
  end;

  function CheckCode128Digits(Index: Integer; CharsLen: Integer): Boolean;
  var
    NumDigits: Integer;
  begin
    Result := False;
    NumDigits := CountCode128Digits(Index);
    if NumDigits mod 2 <> 0 then
    begin
      Characters := Copy(Characters, 1, Index - 1) +
        '0' + Copy(Characters, Index, CharsLen - Index + 1);
      Result := True;
    end;
  end;

  function GetCode128Digits: Integer;
  var
    I: Integer;
    RLen: Integer;
    CurMode: TCode128CodeSubset;
    NeedCharCount: Boolean;
    Skip: Boolean;

  begin
    I := 1;
    Result := Length(Characters);
    RLen := 0;
    CurMode := Code128Subset;
    NeedCharCount := Code128Subset = csCodeC;

    while I <= Result do
    begin
      if (NeedCharCount) and
        (Characters[I] >= '0') and (Characters[I] <= '9') then
      begin
        NeedCharCount := False;
        if CheckCode128Digits(I, Result) then
          Inc(Result);
      end;

      Skip := False;
      if (ExtendedSyntax) and (Characters[I] = '\') and
        (I < Result) then
      begin
        if ((Characters[I + 1] = 'A') or (Characters[I + 1] = 'a')) and
          (CurMode <> csCodeA) then
        begin
          Inc(RLen);
          Digits[RLen] := 101;
          CurMode := csCodeA;
          Skip := True;
        end
        else if ((Characters[I + 1] = 'B') or (Characters[I + 1] = 'b')) and
          (CurMode <> csCodeB) then
        begin
          Inc(RLen);
          Digits[RLen] := 100;
          CurMode := csCodeB;
          Skip := True;
        end
        else if ((Characters[I + 1] = 'C') or (Characters[I + 1] = 'c')) and
          (CurMode <> csCodeC) then
        begin
          NeedCharCount := True;
          Inc(RLen);
          Digits[RLen] := 99;
          CurMode := csCodeC;
          Skip := True;
        end
        else if (Characters[I + 1] = '\') then
        begin
          GetACode128ABDigit('\', I, RLen);
          Skip := True;
        end;
        Inc(I);
      end;

      if not Skip then
        case CurMode of
          csCodeC:
            GetACode128CDigit(Characters[I], I, RLen);
        else
          GetACode128ABDigit(Characters[I], I, RLen);
        end
      else
        Inc(I);
    end;
    Result := RLen;
  end;

var
  I, J: Integer;
  S: string[2];
begin
  FillChar(Digits, SizeOf(Digits), #0);
  Result := 0;

  case BarcodeType of
    bcUPC_A, bcUPC_E, bcEAN_8, bcEAN_13, bcInterleaved2of5:
      begin
        Result := Length(Characters);
        for I := 1 to Result do
          Digits[I] := StrToIntDef(Characters[I], 0);
      end;
    bcCodabar:
      begin
        Result := Length(Characters);
        for I := 1 to Result do
        begin
          case Characters[I] of
            '0'..'9': Digits[I] := StrToIntDef(Characters[I], 0);
            '-': Digits[I] := 10;
            '$': Digits[I] := 11;
            ':': Digits[I] := 12;
            '/': Digits[I] := 13;
            '.': Digits[I] := 14;
            '+': Digits[I] := 15;
            'A', 'a': Digits[I] := 16;
            'B', 'b': Digits[I] := 17;
            'C', 'c': Digits[I] := 18;
            'D', 'd': Digits[I] := 19;
          else
            RaiseError(EStBarCodeError, 'stscInvalidCharacter');
          end;
        end;
      end;
    bcCode11:
      begin
        Result := Length(Characters);
        for I := 1 to Result do
        begin
          case Characters[I] of
            '0'..'9': Digits[I] := StrToIntDef(Characters[I], 0);
            '-': Digits[I] := 10;
          else
            RaiseError(EStBarCodeError, 'stscInvalidCharacter');
          end;
        end;
      end;
    bcCode39:
      begin
        Result := Length(Characters);
        for I := 1 to Result do
        begin
          case Characters[I] of
            '0'..'9': Digits[I] := StrToIntDef(Characters[I], 0);
            'A'..'Z': Digits[I] := Ord(Characters[I]) - Ord('A') + 10;
            '-': Digits[I] := 36;
            '.': Digits[I] := 37;
            ' ': Digits[I] := 38;
            '$': Digits[I] := 39;
            '/': Digits[I] := 40;
            '+': Digits[I] := 41;
            '%': Digits[I] := 42;
            '*': Digits[I] := 43;
          else
            RaiseError(EStBarCodeError, 'stscInvalidCharacter');
          end;
        end;
      end;
    bcCode93:
      begin
        Result := Length(Characters);
        J := 1;
        I := 1;
        while I <= Result do
        begin
          S := Code93Map[Characters[I]];
          if Length(S) > 1 then
          begin
            case S[1] of
              '$': Digits[J] := 43; {(+)}
              '%': Digits[J] := 44; {(%)}
              '/': Digits[J] := 45; {(/)}
              '+': Digits[J] := 46; {(+)}
            else
              RaiseError(EStBarCodeError, 'stscInvalidCharacter');
            end;
            Inc(J);
            S := S[2];
          end;

          case S[1] of
            '0'..'9': Digits[J] := Ord(S[1]) - Ord('0');
            'A'..'Z': Digits[J] := 10 + Ord(S[1]) - Ord('A');
            '-': Digits[J] := 36;
            '.': Digits[J] := 37;
            ' ': Digits[J] := 38;
            '$': Digits[J] := 39;
            '/': Digits[J] := 40;
            '+': Digits[J] := 41;
            '%': Digits[J] := 42;
          else
            RaiseError(EStBarCodeError, 'stscInvalidCharacter');
          end;
          Inc(I);
          Inc(J);
        end;
        Result := J;
      end;
    bcCode128:
      Result := GetCode128Digits;
  end;
end;

procedure GetCheckCharacters(const S: string; var C, K: Integer; BarcodeType: TBarcodeType; var Digits: TStDigitArray; var Count: integer);
var
  I: Integer;
  C1: Integer;
  C2: Integer;
  St: string;

begin
  C := -1;
  K := -1;
  St := S;
  case BarCodeType of
    bcUPC_A:
      begin
        if Length(St) >= 11 then
        begin
          {get digits}
          GetDigits(St, Digits, BarcodeType);
          {determine check character}
          C1 := (Digits[1] + Digits[3] + Digits[5] + Digits[7] +
            Digits[9] + Digits[11]) * 3;
          C2 := Digits[2] + Digits[4] + Digits[6] +
            Digits[8] + Digits[10];
          C := 10 - ((C1 + C2) mod 10);
          if C = 10 then
            C := 0;
        end;
      end;
    bcUPC_E:
      begin
        {get digits}
        GetDigits(St, Digits, BarcodeType);
        {determine check character}
        C1 := (Digits[2] + Digits[4] + Digits[6]) * 3;
        C2 := Digits[1] + Digits[3] + Digits[5];
        C := 10 - ((C1 + C2) mod 10);
        if C = 10 then
          C := 0;
      end;
    bcEAN_8:
      begin
        if Length(St) >= 7 then
        begin
          {get digits}
          GetDigits(St, Digits, BarcodeType);
          {determine check character}
          C1 := (Digits[1] + Digits[3] + Digits[5] + Digits[7]) * 3;
          C2 := Digits[2] + Digits[4] + Digits[6];
          C := 10 - ((C1 + C2) mod 10);
          if C = 10 then
            C := 0;
        end;
      end;
    bcEAN_13:
      begin
        if Length(St) >= 12 then
        begin
          {get digits}
          GetDigits(St, Digits, BarcodeType);
          {determine check character}
          C1 := (Digits[2] + Digits[4] + Digits[6] + Digits[8] +
            Digits[10] + Digits[12]) * 3;
          C2 := Digits[1] + Digits[3] + Digits[5] + Digits[7] +
            Digits[9] + Digits[11];
          C := 10 - ((C1 + C2) mod 10);
          if C = 10 then
            C := 0;
        end;
      end;
    bcInterleaved2of5:
      begin
        {get digits}
        Count := GetDigits(St, Digits, BarcodeType);

        C1 := 0;
        C2 := 0;
        for I := 1 to Count do
          if Odd(I) then
            C1 := C1 + Digits[I] {odd digits}
          else
            C2 := C2 + Digits[I]; {even digits}
        C2 := C2 * 3;

        C := 10 - ((C1 + C2) mod 10);
        if C = 10 then
          C := 0;
      end;
    bcCodabar:
      begin
        {get digits}
        Count := GetDigits(St, Digits, BarcodeType);

        C1 := 0;
        for I := 1 to Count do
          C1 := C1 + Digits[I];

        C := 16 - (C1 mod 16);
        if C = 16 then
          C := 0;
      end;
    bcCode11:
      begin
        {get digits}
        Count := GetDigits(St, Digits, BarcodeType);
        C1 := 0;
        for I := Count downto 1 do
          C1 := C1 + Digits[I] * (Count - I + 1);
        C1 := C1 mod 11; {the "C" check character}
        C2 := C1;
        for I := Count downto 1 do
          C2 := C2 + Digits[I] * (Count - I + 2);
        C2 := C2 mod 11; {the "K" check character}
        K := C2;
        C := C1;
      end;
    bcCode39:
      begin
        {get digits}
        Count := GetDigits(St, Digits, BarcodeType);

        C1 := 0;
        for I := 1 to Count do
          C1 := C1 + Digits[I];

        C := 43 - (C1 mod 43);
        if C = 43 then
          C := 0;
      end;
    bcCode93:
      begin
        {get digits}
        Count := GetDigits(St, Digits, BarcodeType);
        C1 := 0;
        for I := Count downto 1 do
          C1 := C1 + Digits[I] * (Count - I + 1);
        C1 := C1 mod 47; {the "C" check character}
        C2 := C1;
        for I := Count downto 1 do
          C2 := C2 + Digits[I] * (Count - I + 2);
        C2 := C2 mod 47; {the "K" check character}
        K := C2;
        C := C1;
      end;
    bcCode128:
      begin
        {get digits}
        Count := GetDigits(St, Digits, BarcodeType);

        C1 := Digits[1];
        for I := 2 to Count do
          C1 := C1 + Digits[I] * (I - 1);

        C := C1 mod 103;
        if C = 103 then
          C := 0;
      end;
  end;
end;

procedure DrawBarcode(Canvas: TCanvas; DrawInfo: TBarcodeDrawInfo; Info: TBarcodeInfo; PDrawRect: PRect);
var
  I, X, Y: Integer;
  TH, GA, TQ, BB: Integer;
  BarCodeHeight: Integer;
  BarCodeWidth: Integer;
  PixelsPerInchX: Integer;
  TR: TRect;
  SmallestWidth: Double;
  C: string;
  pStart, pEnd: PChar;
  w: Integer;
  DrawRect: TRect;
  PaintRect: TRect;

  function SmallestLineWidth(PixelsPerInch: Integer): Double;
  begin
    Result := PixelsPerInch * 0.010; {10 mils}
    if Result < 1 then
      Result := 1;
  end;

var
  bcBarModWidth: Integer;
  bcSpaceModWidth: Integer;

  bcNormalWidth: Integer;
  bcSpaceWidth: integer;
  bcSupplementWidth: integer;

  procedure CalcBarCodeWidth;
  var
    I: Integer;
  begin
    bcNormalWidth := 0;
    bcSpaceWidth := 0;
    bcSupplementWidth := 0;
    for I := 0 to Info.Count - 1 do
    begin
      if bkSpace in Info[I].Kind then
      begin
        if bkBlankSpace in Info[I].Kind then
          Inc(bcSpaceWidth, bcSpaceModWidth * Info[I].Modules)
        else if bkSupplement in Info[I].Kind then
          Inc(bcSupplementWidth, bcSpaceModWidth * Info[I].Modules)
        else
          Inc(bcNormalWidth, bcSpaceModWidth * Info[I].Modules)
      end
      else
      begin
        if bkBlankSpace in Info[I].Kind then
          Inc(bcSpaceWidth, bcBarModWidth * Info[I].Modules)
        else if bkSupplement in Info[I].Kind then
          Inc(bcSupplementWidth, bcBarModWidth * Info[I].Modules)
        else
          Inc(bcNormalWidth, bcBarModWidth * Info[I].Modules)
      end;
    end;
  end;

  function DrawBar(XPos, YPos, AWidth, AHeight: Integer): Integer;
  var
    aRect: TRect;
  begin
    with Canvas do
    begin
      aRect := Rect(XPos, YPos, XPos + AWidth, YPos + AHeight);
      Canvas.FillRect(aRect);
      //ExcludeRect(Canvas, aRect);
      Result := XPos + AWidth;
    end;

  end;

  function TotalWidth(BW: Integer): Integer;
  var
    ExtraWidth: Integer;
  begin
    with DrawInfo do
    begin
      ExtraWidth := 0;
      if ShowCode and (Code > '') and (BarCodeType = bcUPC_A) then
      begin
        ExtraWidth := Canvas.TextWidth(DrawInfo.Code[1]);
        if SupplementalCode = '' then
          ExtraWidth := 2 * Canvas.TextWidth(Code[Length(Code)]);
      end;
      Result := ExtraWidth + BW;
    end;
  end;

  function GetX(BW: Integer): Integer;
  var
    AllWidth: Integer;
  begin
    with DrawInfo do
    begin
      AllWidth := TotalWidth(BW);
      if Center and (AllWidth < (R.Right - R.Left)) then
        Result := R.Left + ((R.Right - R.Left) - AllWidth) div 2
      else
        Result := R.Left;
    end;
  end;

begin
  //SavedCanvas := SaveDC(Canvas.Handle);
  if DrawInfo.BarcodeType = bcPDF417 then
    DrawPDF417Barcode(Canvas, DrawInfo, Info, PDrawRect)
  else
    with Canvas, DrawInfo do
    begin
      if PDrawRect <> nil then
        PDrawRect^ := Rect(0, 0, 0, 0);
      PixelsPerInchX := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
      {determine narrowest line width}
      SmallestWidth := SmallestLineWidth(PixelsPerInchX);

      {find sizes for the BarCode elements}
      bcBarModWidth := MulDiv(BarWidth, PixelsPerInchX, 1000);
      if bcBarModWidth < BarToSpaceRatio then
        bcBarModWidth := BarToSpaceRatio;
      if bcBarModWidth < SmallestWidth then
        bcBarModWidth := Round(SmallestWidth);
      bcSpaceModWidth := bcBarModWidth div BarToSpaceRatio;

      {total width of BarCode and position within rect}
      CalcBarCodeWidth;
      BarCodeWidth := bcNormalWidth + bcSpaceWidth + bcSupplementWidth;
      BarCodeHeight := R.Bottom - R.Top;

      {get text height}
      TH := TextHeight('Yg0');

      {guard bar adjustment}
      GA := MulDiv(BarCodeHeight, 10, 100); {10% of bar height}
      {but, not more than 1/4 of the font height}
      if ShowCode and (GA > TH div 4) then
        GA := TH div 4; //belal not understand //

      {three quarter height bar adjustment}
      TQ := BarCodeHeight div 4;

      if ShowCode then
      begin
        if GuardBarAbove and GuardBarBelow then
          Dec(BarCodeHeight, TH + TH div 2)
        else if GuardBarAbove or GuardBarBelow then
          Dec(BarCodeHeight, TH + TH div 4)
        else
          Dec(BarCodeHeight, TH)
      end
      else
      begin
        if TallGuardBars then
          Dec(BarCodeHeight, GA * 2);
      end;

      X := GetX(BarCodeWidth);
      Y := R.Top;

      if GuardBarAbove then
        Y := Y + GA;

      //if (GuardBarAbove in Flags) or (GuardBarBelow in Flags) then
        //Dec(BarCodeHeight, GA*2);

      PaintRect := Rect(x, R.Top, x + BarCodeWidth, R.Bottom);

      {draw the text}
      if ShowCode and (Code > '') then
      begin
        C := CorrectBarcodeText(Code, DrawInfo);
        pStart := PChar(C);
        pEnd := pStart + Length(C);

        if BarCodeType = bcUPC_A then
        begin
          {print first and last character to sides of symbol}
          w := TextWidth(C[1]);
          TR.Top := Y;
          TR.Bottom := TR.Top + BarCodeHeight;
          TR.Left := X;
          TR.Right := X + w;
          X := TR.Right;

          PaintRect.Right := PaintRect.Right + w;
          if not CalcRect then
          begin
            DrawString(Canvas, pStart, 1, TR, [txtCenter, txtBottom], @DrawRect);
            //ExcludeRect(Canvas, DrawRect);
          end;
          Inc(pStart);

          {remove character from code to print}
          //C := Copy(C, 2, Length(C) - 1);

          {right hand character - if no supplemental code}
          if SupplementalCode = '' then
          begin
            Dec(pEnd);
            w := TextWidth(pEnd);
            TR.Left := X + bcNormalWidth;
            TR.Right := TR.Left + w;

            PaintRect.Right := PaintRect.Right + w;
            if not CalcRect then
            begin
              DrawString(Canvas, pEnd, 1, TR, [txtLeft, txtBottom], @DrawRect);
              //ExcludeRect(Canvas, DrawRect);
            end;
            {remove character from code to print}
          end;
        end;

        if SupplementalCode <> '' then
        begin
          {draw supplemental code above the code}
          TR.Top := Y + TQ - TH;
          TR.Bottom := Y + BarCodeHeight;
          TR.Left := X + bcNormalWidth + bcSpaceWidth;
          TR.Right := TR.Left + bcSupplementWidth;
          if not CalcRect then
          begin
            DrawString(Canvas, SupplementalCode, TR, [txtMiddle, txtCenter], @DrawRect);
            //ExcludeRect(Canvas, DrawRect)
          end;
        end;

        TR := R;
        TR.Top := R.Bottom - TH;
        TR.Left := X;
        TR.Right := TR.Left + bcNormalWidth;

        if not CalcRect then
        begin
          DrawString(Canvas, pStart, pEnd - pStart, TR, [txtCenter, txtMiddle], @DrawRect);
          //ExcludeRect(Canvas, DrawRect);
        end;

      end; //show code

      if not CalcRect then
      begin
        Brush.Color := BarColor;
        Brush.Style := bsSolid;
        if (BarCodeType = bcInterleaved2of5) and BearerBars then
        begin
          BB := 3 * bcBarModWidth;
          {reduce height to allow for bearer bars}
          Dec(BarCodeHeight, BB * 2);
          {draw the bearer bars}
          Rectangle(X - bcBarModWidth, Y, X + BarCodeWidth + bcBarModWidth, Y + BB);
          Rectangle(X - bcBarModWidth, Y + BarCodeHeight + BB, X + BarCodeWidth + bcBarModWidth, Y + BarCodeHeight + BB * 2);
          {adjust top of BarCode}
          Inc(Y, BB);
        end;

        {draw the bar code}
        for I := 0 to Info.Count - 1 do
        begin
          if bkSpace in Info[I].Kind then
            Inc(X, bcSpaceModWidth * Info[I].Modules)
          else if (bkGuard in Info[I].Kind) and TallGuardBars then
          begin
            if GuardBarAbove and GuardBarBelow then
              X := DrawBar(X, Y - GA, bcBarModWidth * Info[I].Modules, BarCodeHeight + 2 * GA)
            else if GuardBarAbove then
              X := DrawBar(X, Y - GA, bcBarModWidth * Info[I].Modules, BarCodeHeight + GA)
            else if GuardBarBelow then
              X := DrawBar(X, Y, bcBarModWidth * Info[I].Modules, BarCodeHeight + 2 * GA)
          end
          else if (bkBar in Info[I].Kind) or (bkGuard in Info[I].Kind) then
            X := DrawBar(X, Y, bcBarModWidth * Info[I].Modules, BarCodeHeight)
          else if (bkThreeQuarterBar in Info[I].Kind) then
            X := DrawBar(X, Y + TQ, bcBarModWidth * Info[I].Modules, BarCodeHeight - TQ);
        end;
        //RestoreDC(Canvas.Handle, SavedCanvas);
        //FillRect(PaintRect);
        //ExcludeRect(Canvas, PaintRect);
      end;
      if PDrawRect <> nil then PDrawRect^ := PaintRect;
    end;
end;

function CorrectBarcodeText(const vText: string; DrawInfo: TBarcodeDrawInfo): string;
var
  Digits: TStDigitArray;
  Count: Integer;
  CheckC: Integer;
  CheckK: Integer;
  i: Integer;
begin
  with DrawInfo do
  begin
    Result := vText;
    {fill out invalid codes}
    case BarcodeType of
      bcUPC_A:
        begin
          Result := Copy(Result, 1, 12); {truncate}
          if Length(Result) = 11 then
          begin
            GetCheckCharacters(Result, CheckC, CheckK, BarcodeType, Digits, Count);
            Result := Result + IntToStr(CheckC);
          end;
          while Length(Result) < 12 do
            Result := Result + '0';
        end;
      bcUPC_E:
        begin
          Result := Copy(Result, 1, 6); {truncate}
          while Length(Result) < 6 do
            Result := Result + '0';
        end;
      bcEAN_8:
        begin
          Result := Copy(Result, 1, 8); {truncate}
          if Length(Result) = 7 then
          begin
            GetCheckCharacters(Result, CheckC, CheckK, BarcodeType, Digits, Count);
            Result := Result + IntToStr(CheckC);
          end;
          while Length(Result) < 8 do
            Result := Result + '0';
        end;
      bcEAN_13:
        begin
          Result := Copy(Result, 1, 13); {truncate}
          if Length(Result) = 12 then
          begin
            GetCheckCharacters(Result, CheckC, CheckK, BarcodeType, Digits, Count);
            Result := Result + IntToStr(CheckC);
          end;
          while Length(Result) < 13 do
            Result := Result + '0';
        end;
      bcInterleaved2of5:
        begin
          if Odd(Length(Result)) then
            Result := '0' + Result;
        end;
      bcCodabar:
        begin
          if ShowGuardChars then
            {strip leading and trailing characters}
            Result := Copy(Result, 2, Length(Result) - 2);
        end;
      bcCode11:
        begin
        end;
      bcCode39:
        begin
          {add guard characters}
          if Result[1] <> '*' then
            Result := '*' + Result;
          if Result[Length(Result)] <> '*' then
            Result := Result + '*';
          if ShowGuardChars then
            {strip leading and trailing characters}
            Result := Copy(Result, 2, Length(Result) - 2);
        end;
      bcCode93:
        begin
          {remove non-printable characters}
          for I := 1 to Length(Result) do
            if Result[I] < ' ' then
              Result[I] := ' ';
        end;
      bcCode128:
        begin
          {remove non-printable characters}
          I := 1;
          while I <= Length(Result) do
          begin
            if Result[I] < ' ' then
              Result[I] := ' ';
            if (i < Length(Result)) and ExtendedSyntax then
            begin
              if (Result[I] = '\') and
                (Result[I + 1] in ['A', 'B', 'C', 'a', 'b', 'c']) then
              begin
                Result[I] := ' ';
                Result[I + 1] := ' ';
                Inc(I);
              end
              else if (Result[I] = '\') and (Result[I + 1] = '\') then
              begin
                Result[I] := ' ';
                Inc(I);
              end;
            end;
            Inc(I);
          end;
        end;
    end;
  end;
end;

procedure RaiseError(E: ExceptClass; Msg: string);
begin
end;

end.

<?php
/***********************************************************************

  Copyright ($c) 2009  zaher dirkey (zaher@parmaja.com)

  This file is part of cssFlip.

  cssFlip is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation;

  cssFlip is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
  MA  02111-1307  USA

************************************************************************/

  define('S_TEXT', 0);
  define('S_STATMENT', 1);
  define('S_SELECTOR', 2);
  define('S_PROPERTY', 3);
  define('S_COMMENT', 4);
  define('S_STRING', 5);
  
  define('U_CASE_NONE', 0);
  define('U_CASE_UPPER', 1);
  define('U_CASE_LOWER', 2);

//default values
$USE_CASE= U_CASE_NONE;
$USE_MULTILINE= 0;
$USE_LAST_LINE_SEMICOLON = false;
$USE_EOL = "\n";
$USE_TABCHAR = '    ';//or "\t"
$USE_TAB_END_BLOCKS = '';//or '    ' or "\t"
$USE_NOFLIP = false; //false mean just format the files only no flip


function use_case($text) {
  global $USE_CASE;
  if ($USE_CASE==U_CASE_UPPER)
    return strtoupper($text);
  elseif ($USE_CASE==U_CASE_LOWER)
    return strtolower($text);
  else
    return $text;
}

function str_explode($c, $s)
{
  $p = strpos($s, $c);
  $a = array();
  if ($p!==false)
  {
    $a[] = substr($s, 0, $p);
    $a[] = substr($s, $p+1);
  }
  else
  {
    $a[] = $s;
    $a[] = '';
  }
  return $a;
}
  class CssFile {
    var $blocks = array();
    var $block;
    var $comment = '';
    var $text = '';
    var $properties = array();
    var $output;
    var $left_count = 0;
    var $right_count = 0;

    function write($out)
    {
      if (isset($this->output))
        fwrite($this->output, $out);
      else
        echo $out;
    }

    function open()
    {
    }

    function close()
    {
    }

    function flush_out()
    {
      $this->write($this->text);
      $this->text = '';
    }

    function exchange($value)   // if it has 4 value i exchange the 2 and 4 values
    {
      $a=explode(' ', $value);
      if (count($a) == 4)
      {
        $t=$a[3];
        $a[3]=$a[1];
        $a[1]=$t;
      }
      else
        return $value;
      
      $r='';
      foreach ($a as $v)
      {
//        $v=use_case($v);
        $r=$r.' '.$v;
      }
      return $r;
    }

    function change_value($value)
    {
      $a=explode(' ', $value);
      $r='';
      foreach ($a as $v)
      {
        $v=strtoupper($v);
        if ($v == 'RIGHT')
          $v= 'LEFT';
        else if ($v == 'LEFT')
          $v= 'RIGHT';
          
        if (empty($r))
          $r=use_case($v);
        else
          $r=$r.' '.use_case($v);
      }
      return $r;
    }

    function flip_value($name, $value)
    {
      global $USE_NOFLIP;
      $new_value = trim($value);
      $value = strtoupper($new_value);
      $name = strtoupper($name);
      if ($USE_NOFLIP === false) {
        if ($name == 'DIRECTION')
        {
          if ($value=='LTR')
            $new_value=use_case('RTL');
          else if ($value=='RTL')
            $new_value=use_case('LTR');
        }
        //BORDER
        else if ($name == 'LEFT')
          $name='RIGHT';
        else if ($name == 'RIGHT')
          $name='LEFT';
        else if ($name == 'BORDER-RIGHT')
          $name='BORDER-LEFT';
        else if ($name == 'BORDER-LEFT')
          $name='BORDER-RIGHT';
        else if ($name == 'BORDER-LEFT-WIDTH')
          $name='BORDER-RIGHT-WIDTH';
        else if ($name == 'BORDER-RIGHT-WIDTH')
          $name='BORDER-LEFT-WIDTH';
        else if ($name == 'BORDER-LEFT-STYLE')
          $name='BORDER-RIGHT-STYLE';
        else if ($name == 'BORDER-RIGHT-STYLE')
          $name='BORDER-LEFT-STYLE';
        else if ($name == 'BORDER-LEFT-COLOR')
          $name='BORDER-RIGHT-COLOR';
        else if ($name == 'BORDER-RIGHT-COLOR')
          $name='BORDER-LEFT-COLOR';
        else if ($name == 'BORDER-STYLE')
          $new_value=$this->exchange($new_value);
        else if ($name == 'BORDER-WIDTH')
          $new_value=$this->exchange($new_value);
        else if ($name == 'BORDER-COLOR')
          $new_value=$this->exchange($new_value);
        //MARGIN
        else if ($name == 'MARGIN')
          $new_value=$this->exchange($new_value);
        else if ($name == 'MARGIN-RIGHT')
          $name='MARGIN-LEFT';
        else if ($name == 'MARGIN-LEFT')
          $name='MARGIN-RIGHT';
        //PADDING
        else if ($name == 'PADDING')
          $new_value=$this->exchange($new_value);
        else if ($name == 'PADDING-RIGHT')
          $name='PADDING-LEFT';
        else if ($name == 'PADDING-LEFT')
          $name='PADDING-RIGHT';
        //OTHERS
        else if ($name == 'BACKGROUND')
          $new_value=$this->change_value($new_value);
        else if ($name == 'BACKGROUND-POSITION')
          $new_value=$this->change_value($new_value);
        else if ($name == 'FLOAT')
          $new_value=$this->change_value($new_value);
        else if ($name == 'TEXT-ALIGN')
          $new_value=$this->change_value($new_value);
        else if ($name == 'CLEAR')
          $new_value=$this->change_value($new_value);
        else if ($name == 'BORDER-COLOR')
          $new_value=$this->exchange($new_value);
        else {
          if (strpos($name, 'LEFT')!==false)
          {
            echo $name.': '.$value."\n";
            $this->left_count++;
          }
          if (strpos($name, 'RIGHT')!==false)
          {
            echo $name.': '.$value."\n";
            $this->right_count++;
          }
          if (strpos($value, 'LEFT')!==false)
          {
            echo $name.': '.$value."\n";
            $this->left_count++;
          }
          if (strpos($value, 'RIGHT')!==false)
          {
            echo $name.': '.$value."\n";
            $this->right_count++;
          }
        }
      }
      return use_case($name).': '.$new_value;
    }

    function flush_properties()
    {
      global $USE_MULTILINE, $USE_TABCHAR, $USE_EOL, $USE_LAST_LINE_SEMICOLON, $USE_TAB_END_BLOCKS;
      
      $prop_count = count($this->properties);
      if (($prop_count <= $USE_MULTILINE))
      {
        $pref = "";
        $suff = "";
      }
      else
      {
        $pref = $USE_TABCHAR;
        $suff = $USE_EOL;
        $this->write($suff);
      }

      $c = 1;
      foreach ($this->properties as $v)
      {
        if (($c < $prop_count) or ($USE_LAST_LINE_SEMICOLON===true))
          $this->write($pref.$v.';'.$suff);
        else
          $this->write($pref.$v.$suff);
        $c++;
      }
      $this->write($USE_TAB_END_BLOCKS.'}');
      $this->properties = array();
    }

    function add_property()
    {
      global $USE_EOL;
      $this->text = trim($this->text);
      if (!empty($this->text))
      {
        $this->text = str_replace($USE_EOL, '', $this->text);
        list($name, $value)=str_explode(':', $this->text);
        $name=trim($name);
        $value=trim($value);
        $this->properties[] = $this->flip_value($name, $value);
      }
      $this->text = '';
    }

    function push_block($state)
    {
      $this->blocks[]= $this->block;
      $this->block = $state;
    }

    function pop_block()
    {
      if (empty($this->blocks))
        $this->block = S_TEXT;
      else
      {
        $this->block = array_pop($this->blocks);
      }
    }

    function parse($line) {
//      DebugBreak();
      $i = 0;
      while ($i < strlen($line))
      {
        $cur_chr = $line{$i};
        if (($i + 1) < strlen($line))
          $next_char = $line{$i + 1};
        else
          $next_char = '';

        if (($this->block!=S_COMMENT) and ($cur_chr == '/') and ($next_char == '*'))  // close by '*/'
        {
          $i++;
          $this->text .= $cur_chr.$next_char;
          $this->push_block(S_COMMENT);
        }
        else
          switch ($this->block) {
            case S_TEXT:
            {
              if ($cur_chr == '@') // close by ';'
              {
                $this->push_block(S_STATMENT);
                $this->text .= $cur_chr;
              }
              else if ($cur_chr == '{') // close by '}'
              {
                $this->write('{');
                $this->push_block(S_PROPERTY);
              }
              else
                $this->write($cur_chr);
              break;
            }

            case S_STATMENT:
            {
              $this->text .= $cur_chr;
              if ($cur_chr == ';')
              {
                $this->flush_out();
                $this->pop_block();
              }
              break;
            }

            case S_PROPERTY:
            {
              if ($cur_chr == '}')
              {
                $this->add_property();
                $this->flush_properties();
//                $this->write('}');
                $this->pop_block();
              }
              elseif ($cur_chr == ';')
              {
                $this->add_property();
              }
              else
                $this->text .= $cur_chr;
              break;
            }

            case S_COMMENT:
            {
              if (($cur_chr == '*') and ($next_char == '/'))
              {
                $i++;
                $this->text .= $cur_chr.$next_char;
                $this->flush_out();
                $this->pop_block();
              }
              else
                $this->text .= $cur_chr;
              break;
            }
          }
        $i++;
      }
    }
  }

function css_switch_file($file_in, $file_out = '')
{
  $f = fopen($file_in, 'r');
  if ($f!==false)
  {
    $css = new CssFile;
    if (!empty($file_out))
      $css->output = fopen($file_out, 'w');
    $css->open();
    while (!feof($f))
    {
      $line = fgets($f);
      echo $css->parse($line);
    }
    $css->close();
    fclose($f);
    if (isset($css->output))
      fclose($css->output);
    return array($css->left_count, $css->right_count);
  }
}

?>
<?php
/***********************************************************************

  Copyright (C) 2009  zaher dirkey (zaher@parmaja.com)

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

require 'cf_classes.php';
require 'cf_config.php';

function check_dir($dir)
{
  $dir = str_replace('\\','/',$dir);
  if (substr($dir, -1) != '/')
    $dir.='/';
  return $dir;
}

function css_switch_dir($dir_in, $dir_out)
{
  $dh=opendir($dir_in);
  $dir_out = check_dir($dir_out);
  $dir_in = check_dir($dir_in);
  @mkdir($dir_out);
  while (($file = readdir($dh)) !== false) {
    if ($file!='.' and $file!='..' and is_dir($dir_in.$file))
      css_switch_dir($dir_in.$file, $dir_out.$file);
    else if (is_file($dir_in.$file))
    {
      if (preg_match("/\.(css)$/", $file))
      {
        echo 'flip '.$dir_out.$file."\n";
        list($lc, $rc) = css_switch_file($dir_in.$file, $dir_out.$file);
        echo " Left rest= ".$lc.'   Right rest= '.$rc.' ';
      }
      else
      {
        echo 'copy '.$dir_out.$file;
        copy($dir_in.$file, $dir_out.$file);
      }
      echo " done \r\n";
    }
  }
  closedir($dh);
}

$root = $argv[0];
$cmd = array(); //command not switches (not started with -)
$swt = array(); //switches

if (!isset($argc) or ($argc < 1)) //2 mean one paramrter included command params
{
  echo 'Type input file/folder :';
  $in = rtrim(fgets(STDIN, 1024));
  echo 'Type ouput file/folder :';
  $out = rtrim(fgets(STDIN, 1024));
}
else
{
  foreach($argv as $arg) {
    if (($arg{0} == '-'))
      $swt[] = strtoupper(substr($arg, 1));
    else
      $cmd[] = $arg;
  }
  array_shift($cmd);//remove first cmd

  if (in_array('NOFLIP', $swt))
    $USE_NOFLIP = true;
//  var_dump($swt);
//  var_dump($cmd);
  $in = $cmd[0];
  if (count($cmd) > 1)
    $out = $cmd[1];
  else
    $out='';
}

if (is_dir($in))
{
  if (empty($out) or is_file($out))
    echo 'error out dir not defined or it\'s a file';
  else
  {
    css_switch_dir($in, $out);
  }
}
else
{
  if (!file_exists($in))
    echo 'File not found '.$in;
  else
    css_switch_file($in, $out);
}
?>
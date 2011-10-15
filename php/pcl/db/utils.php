<?php

function db_quote($value) {
  return "'" .$value ."'";
}

function db_insert($table_name, $params, $key_field = null) {
  $sql = "insert into " .$table_name ." (%s)" ." values(%s)";
  foreach ($params as $field => $value) {
    if ($field != $key_field) {
      if (!isset($fields))
        $fields = $field;
      else
        $fields = $fields ."," .$field;
      if (!isset($values))
        $values = $value;
      else
        $values = $values ."," .$value;
    }
  }
  $sql = sprintf($sql, $fields, $values);
  return $sql;
}

function db_update($table_name, $params, $keys) {
 
	$w = '';
  $f = '';
  foreach ($keys as $key => $value) {
    if (!empty($w))
      $w  = $w . ",";
    $w = $w . $key ."=" .$value;
  }
  
  foreach ($params as $field => $value) {
    if (!empty($f))
      $f  = $f . ",";
    $f = $f . $field ."=" .$value;    
  }
  
  $sql = "update " .$table_name ." set " . $f;
  if (!empty($w))
    $sql = $sql . ' where ' . $w;
  return $sql;
}

?>
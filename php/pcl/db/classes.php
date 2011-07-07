<?php
/**
 * PCL/DB 
 *
 * @license    PPL (http://www.parmaja.com/licenses/ppl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @author    Jihad Khalifa <jihad at parmaja dot com>
 **/

if (!defined('ROOT'))
  exit('You must define a ROOT');

if (!defined('PCL_ROOT'))
  exit('You must define a PCL_ROOT');
  
function new_connection($db_type, $name, $username = '', $password = '', $host = '', $prefix = '', $permanent = false)
{
  $db_class = $db_type . '_connection_class';
  $db_classfile = PCL_ROOT.'db/'.$db_type.'.php';
  require($db_classfile);
  $connection = new $db_class($db_type, $host, $username, $password, $name, $prefix, $permanent);
  return $connection;
}

/**
  connection_class
*/

class connection_class
{
  var $type = '';
  var $dbname = '';
  var $host = '';
  var $prefix = '';
  var $username = '';
  var $password = '';
  var $permanent = false;
  var $save_history = false;
  var $history= array();
  var $lower_fields = false;//make all fields to lower case for fetch_assoc 

  var $handle;
  
  function connection_class($db_type, $host, $username, $password, $name, $prefix, $permanent, $save_history = false)
  {
    $this->save_history = $save_history;
    $this->type = $db_type;
    $this->host = $host;
    $this->username = $username;
    $this->password = $password;
    $this->name = $name;
    $this->prefix = $prefix;
    $this->permanent = $permanent;
  }
  
  function error($error)
  {
  }

  function connect()
  {
    return $this->do_connect();
  }

  function disconnect()
  {
    $this->do_disconnect();
    unset($this->handle);
  }

  function new_session()
  {
    $session  = $this->do_new_session();
    $session->connection = $this;
    return $session;
  }
}

/**
  session_class
  
  Not all database support sessions or muliple transactions in one connection
*/

class session_class
{
  var $handle;

  function new_command()
  {
    $command = $this->do_new_command();
    $command->session =$this;
    $command->connection =$this->connection;
    return $command;
  }

  function start()
  {
    if (!isset($this->connection->handle))
      $this->connection->connect();
    return $this->do_start();
  }
  
  function commit($retain = false)
  {
    $this->do_commit($retain);
    unset($this->handle);
  }
  
  function rollback($retain = false)
  {
    $this->do_rollback($retain);
    unset($this->handle);
  }

  function run($sql)
  {
    $cmd = $this->new_command();
    $cmd->run($sql);
    return $cmd;
  }

  function query($sql)
  {
    $cmd = $this->new_command();
    $cmd->query($sql);
    return $cmd;
  }

  function install($script)
  {
    $this->do_before_install();

    $file = file_get_contents($script);
    $scripts = preg_split("#\^#", $file, -1, PREG_SPLIT_NO_EMPTY);
    $a[]='#__prefix__#';
    $b[]=$this->connection->prefix;
    foreach ($scripts as $key => $sql)
    {
      $sql = preg_replace($a, $b, $sql);
      $this->run($sql) or
        error('Unable to run script '.$key."\n".$sql."\n".'. Please check your settings and try again.',  __FILE__, __LINE__, $this->error());
    }
    $this->commit_retain();
  }
}

/**
  command_class
  
*/

class command_class
{
  var $handle;
  var $result;
  var $insert_id;
  var $session;
  var $connection;
  var $prepared = false;
  var $fetch_blobs = false;
  var $sql;
  var $fields;//result of fetch
  
  function run($sql = '', $field_id = null)
  {
    if (empty($sql))
      $sql = $this->sql;
    if (!empty($sql)) {
      if ($this->connection->save_history)
        $this->connection->history[] = $sql;
    }
    
    $this->handle = $this->do_exec($sql, true, $field_id);
    return $this->handle;    
  }

  function prepare($sql = '')
  {
    if (!empty($sql))
      $this->sql = $sql;
      
    if (!empty($this->sql)) {
      if ($this->connection->save_history)
        $this->connection->history[] = $this->sql;
    }

    $this->handle = $this->do_prepare($this->sql);
    $this->prepared = true;//or check if good prepared
    return $this->prepared;
  }

  function execute($next_when_execute = true, $params = null)
  {
    if ($this->prepared===false)
      $this->prepare($this->sql);
      
    $this->eof = false;  
    $r = $this->do_execute($params);
    if ($r and $next_when_execute) {
      $r = $this->next();      
    }
    return $r;
  }
  
  function query($sql = '', $field_id = null)
  {
    if (empty($sql))
      $sql = $this->sql;
    if (!empty($sql)) {
      if ($this->connection->save_history)
        $this->connection->history[] = $sql;
    }
    
    $this->handle = $this->do_execute($sql, false, $field_id);
    return $this->handle;
  }

  function fetch()
  {
    return $this->do_fetch();
  } 
  
  function next()
  {
    $this->fields = $this->fetch();
    $this->eof = empty($this->fields);
    return !$this->eof;
  } 

  function close()
  {
    unset($this->fields);
    $this->eof = false;
    $this->prepared = false;
    return $this->do_close();
  }
  
  function csv_fields($comma = ';') {
    $r = '';
    foreach ($this->fields as $key => $value) {
      if (!empty($r)) 
        $r = $r.$comma;         
      $r = $r.$value;      
    }    
   return $r;
  }

  function csv_columns($comma = ';') {
    $r = '';
    foreach ($this->fields as $key => $value) {
      if (!empty($r)) 
        $r = $r.$comma;         
      $r = $r.$key;      
    }    
    return $r;
  }
}
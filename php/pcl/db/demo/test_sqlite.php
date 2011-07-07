<?php
/**
 *
 * @license    PPL (http://www.parmaja.com/licenses/ppl.html)
 **/

define('ROOT', realpath(dirname(__FILE__)));
define('PCL_ROOT', realpath(dirname(__FILE__).'/../../').'/');

include(PCL_ROOT.'db/classes.php');

$db_type = 'sqlite';
$host = '';
$username = '';
$password = '';
$name = 'data.sqlite';
$prefix = '';
$permanent = false;        

$connection = new_connection($db_type, $name);

$connection->connect();

$session = $connection->new_session();

$session->start();

$cmd = $session->new_command();

$cmd->sql = 'select * from "Requests"';

$cmd->execute();

$result = $cmd->fetch();

while ($result) {
  echo '> '.$result['ReqName']."\n";
  $result = $cmd->fetch();
}

$cmd->close();

$session->commit();
?>
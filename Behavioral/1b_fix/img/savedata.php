<?php
$user="moral";
$password="j|n321";
$database="adam";
mysql_connect("localhost",$user,$password);
@mysql_select_db($database) or die("Unable to select database");

// get the table name
$table = $_POST[table];

// decode the data object from json
$data = json_decode($_POST[json], true);

//echo(print_r($data,true));

$values = array_map('mysql_real_escape_string', array_values($data));
$keys = array_keys($data);

$query = 'INSERT INTO `'.$table.'` (`'.implode('`,`', $keys).'`) VALUES (\''.implode('\',\'', $values).'\')';
//echo($query);
$result = mysql_query($query);

if (strcmp($table, "counterbalance") == 0) {
    $query2 = 'SELECT * FROM `'.$table.'` WHERE `subject` = "'.$values[0].'"';
    //echo($query2);
    $result2 = mysql_query($query2);
    //if ($result2 == FALSE) die(mysql_error());
    $result2 = mysql_fetch_assoc($result2);
    echo($result2["id"]);
}

// confirm the results
if (!$result) {
    die('Invalid query: ' . mysql_error());
} else {
    //print "successful insert!";
}

mysql_close();
?>
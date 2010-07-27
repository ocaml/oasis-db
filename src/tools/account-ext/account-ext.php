<?php 
require_once 'conf.php';

$action = $_GET['action'];
$user_id = $_GET['user_id'];
$redirect = $_GET['redirect'];

if ($redirect == '')
{
	unset($redirect);
};

$cookie_name = 'account_ext_token';

$dbconn = pg_connect($pg_connect_string);

// Check if we are already logged in

$account_ext_token = $_COOKIE[$cookie_name];
if (isset($account_ext_token))
{
	// Check in the DB 
	$result = pg_query_params 
		($dbconn, 
		"SELECT user_id FROM tokens WHERE token = $1 AND expire > NOW ()",
		array($account_ext_token));

	while ($row = pg_fetch_row($result))
	{
		$user_id = $row[0];
	};
};


// Some GC stuff 

if (!pg_query($dbconn, "DELETE FROM tokens WHERE expire <= NOW ()"))
{
	echo "Unable to run GC";
	exit;
};

function intro ()
{
?>
<html>
<head>
<title>3rd party login test</title>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
</head>
<body>
<?php
}

function outro () 
{
?>
</body>
</html>
<?php
}

function token () 
{
	$str = "";
	for ($i = 0; $i < 30; $i++)
	{
		$str .= chr(rand(0,255));
	}

	return base64_encode($str);
}

function info_or_redirect ($msg)
{
	global $redirect;
	if (isset($redirect))
	{
		header("Location: $redirect");
	}
	else
	{
		intro ();
		echo "<p>$msg</p>";
		outro ();
	}
};

function cookie_account_ext ($set)
{
	global $cookie_name, $dbconn, $user_id;

	// Delete previous entry 
	if (!pg_query_params ("DELETE FROM tokens WHERE user_id = $1", array($user_id)))
	{
		echo "Cannot remove previous token";
		exit;
	};

	if ($set)
	{
		$new_token = token();

		// Insert into DB 
		$result = pg_query_params 
			($dbconn,
			"INSERT INTO tokens (token, user_id, expire) VALUES ($1, $2, now () + interval '1 hour')",
			array($new_token, $user_id));
		if (!$result)
		{
			echo "An error occurs";
			exit;
		}
	}
	else
	{
		$new_token = '';
	}

	if (!setcookie($cookie_name, $new_token, time()+3600))
	{
		echo "Cannot set cookie";
		exit;
	}

	if (!setcookie($cookie_name, $new_token, time()+3600, '/'))
	{
		echo "Cannot set cookie";
		exit;
	}
}

switch ($action) 
{
case 'login':
	if (isset($user_id))
	{
		cookie_account_ext(true);
		info_or_redirect("You are now logged in");
	}
	else
	{
		# Login screen
		intro ();
?>
	<form>
	<select name="user_id">
	<?php
		$result=pg_query($dbconn, "SELECT user_id, user_name FROM users");
		while ($row = pg_fetch_row($result)) 
		{
			echo '<option value="'.$row[0].'">'.$row[1].'</option>';
		}
	?>
	</select>
	<input type="submit" value="Login" /><br />
	<?php
		foreach (array("action", "redirect") as $value)
		{
			echo $value.': '.$_GET[$value].'<br />';
			echo '<input type="hidden" name="'.$value.'" value="'.$_GET[$value].'" />';
		}
	?>
	</form>
<?php
		outro ();
	};
	break;

case 'logout':
	cookie_account_ext(false);
	info_or_redirect("You are now logged out");
	break;

case 'manage':
	intro ();
	echo '<p>Manage account</p>';
	outro ();
	break;

case 'new':
	intro ();
	echo '<p>New account</p>';
	outro ();
	break;

case 'lost_passwd':
  intro ();
  echo '<p>Lost password</p>';
  outro ();
  break;

default: 
	intro ();
	echo '<p>Unknow action</p>';
	outro ();
	break;
}

pg_close($dbconn);

?>

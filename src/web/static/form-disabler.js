
/*
 * Disable all forms when a form is edited
 *
 * authors: Sylvain Le Gall
 */

var disabled = new Array ();

function getFormElements ()
{
  var input_list = document.getElementsByTagName("input");
  var textarea_list = document.getElementsByTagName("textarea");
  var button_list = document.getElementsByTagName("button");
  var result = new Array();

  for (var i = 0; i < input_list.length; i++)
  {
    result.push(input_list[i]);
  };
  for (var i = 0; i < textarea_list.length; i++)
  {
    result.push(textarea_list[i]);
  };
  for (var i = 0; i < button_list.length; i++)
  {
    result.push(button_list[i]);
  };

  return result;
};

function disableAllExcept (self_form)
{
  var node_list = getFormElements();
  for (var i = 0; i < node_list.length; i++)
  {
    var node = node_list[i];
    if (node.type != 'hidden' && node.type != 'select' && node.form != self_form)
    {
      node.disabled = true;
      disabled.push(node);
    };
  };
};

function reEnable ()
{
  while (disabled.length > 0)
  {
    var node = disabled.pop();
    node.disabled = false;
  };
};

function setDisableHandler ()
{
  var node_list = getFormElements();
  for (var i = 0; i < node_list.length; i++)
  {
    var node = node_list[i];
    if (node.type == 'reset')
    {
      node.onclick = function () {reEnable ();};
    }
    else
    {
      node.onchange = function () {disableAllExcept(this.form);};
      node.onkeypress = function () {disableAllExcept(this.form);};
    };
  };
};

setDisableHandler();

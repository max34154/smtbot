var EMAIL_SUBJECT = "Код подтверждения учетной записи SM Telegram Bot"
var EMAIL_MESSAGE = "1.Откройте чат с SMEAISTBot<br> 2.Введите код с помощью команды /code $$CODE$$<br>"

function getContactByKey(key)
{
    var email;
    var active_count = 0;
    lib.c.$("contacts").
    select('email="' + key + '"').
    iterate(function(item)
    {

        if (item.active === true)
        {
            active_count++;
            email = item.email;
        }
    })
    return {
        email: email,
        type: "user",
        active_count: active_count
    }
}


function getOperatorByKey(key)
{
    var email;
    var active_count = 0;
    lib.c.$("operator").
    select('email="' + key + '"').
    iterate(function(item)
    {
        if (!item.man_lockout_user)
        {
            active_count++;
            email = item.email;
        }
    })
    return {
        email: email,
        type: "operator",
        active_count: active_count
    }
}

function processUser(user, activation_requred)
{
    if (user.active_count == 0)
    {
        if (user.email)
        {
            vars.$L_file.active = 'B'
            vars.$L_file.user_type = user.type;
        }
        else
        {
            vars.$L_file.active = 'N'
            vars.$L_file.user_type = 'unknown';
        }
        return 1;
    }
    if (user.active_count > 1)
    {
        vars.$L_file.active = 'M'
        vars.$L_file.user_type = user.type;
        return 1;
    }
    vars.$L_file.active = activation_requred ?
        'C' + activation(user.email) : 'A';
    vars.$L_file.user_type = user.type;
    return 0;
}


/*
;;  status - current user status 
;;            A - active, confirmed 
;;            T - temporary blocked 
;;            B - blocked, 
;;            N - not-found 
;;            M - non-unique record 
;;            C<ConfirmationCode> - active, confirmation required
;;  role  - only for active confirmed 
;;         user  - listed in sm contacts 
;;         operator - has sm operator record 
*/

function activation(email)
{
    var code = (Math.floor(Math.random() * 10000)+10000).toString().substring(1);
    send_email(email, "sm", EMAIL_SUBJECT, EMAIL_MESSAGE.replace('$$CODE$$', code));
    return code;
}

function getTBOTUser(request)
{
    var activation = request.active
    if (processUser(getOperatorByKey(request.contact_name), request.active))
        processUser(getContactByKey(request.contact_name), activation);
    vars.$L_exit = "normal";
}



function send_email(email, oper, subj, message)
{
    var id = new SCDatum();
    var rt = new SCDatum();
    system.functions.rtecall("getnumber", rt, id, "event", "");
    if (id.getText().length == 0) return -2;

    var m = new SCFile("eventout");
    var d = new Date();
    m.evtype = "email";
    m.evtime = d;
    m.evexpire = d;
    m.evsysseq = id.getText();
    m.evsepchar = "^";
    var body = '<html><table style="width: 600px" border=1 cellpadding="6" cellspacing="2">' +
        '<tr><td>' + message + '</td></tr></table></br></html>';
    m.evfields = email + ';^' + oper + '^' + email + '^' + subj + '^' + body;
    var mc = m.doSave();
    if (mc == RC_SUCCESS) return 0;
    else return -1;
}
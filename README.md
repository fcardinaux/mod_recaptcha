mod_recaptcha
=============

A module for showing a reCAPTCHA on Zotonic sites. 

Original author: François Cardinaux, CH 1207 Geneva (http://bit.ly/qTaona)

Installation
------------

1. Clone this module into your Zotonic modules directory.
2. Enable the module in the Zotonic admin, and click the ‘Configuration’ button.
   Enter your [reCAPTCHA site key and secret](https://developers.google.com/recaptcha/docs/start).

Usage
-----

### On signup forms

    Edit your signup form template (typically signup.tpl) and add the following
    line where you want the captcha to appear: 
 
        {% include "_captcha.tpl" %}

### On other forms

1. In your form template, include the proper template where you want the CAPTCHA
   to appear:

   For reCAPTCHA 1:
        
    ```dtl
    {% include "captcha.tpl" %}
    ```
    
    For reCAPTCHA 2:
    
    ```dtl
    {% include "_recaptcha2.tpl" %}
    ```

2. Where you process the form submission, add: 
 
    ```erlang
    % ...
    case mod_recaptcha:check_recaptcha(Context) of
        ok -> 
            process_form(Context);
        {error, Error} ->
            display_error_message(Error)
    end,
    % ...
            
    % Implementation not shown here
    process_form(_Context) ->
        todo.
    
    % Implementation not shown here
    display_error_message(_Error) ->
        todo.
    ```
             
### Disabling recaptcha with an observer

    Why
    ...

    By default, recaptcha is enabled if and only if the module mod_recaptcha is 
    enabled. 
    
    In some situations however, you may want to disable recaptcha 
    programmatically, depending on some variables of your system. For instance, 
    suppose that you are starting a membership-based website, and that during 
    the initial period you want to restrict the use of your signup form to 
    invited people only. In this case, you don't want to annoy them with a 
    captcha, since their e-mail address is checked against a control list 
    anyway. However, you want that recaptcha remains enabled on other forms
    of the site, e.g. your contact form.
    
    How
    ...

    The simplest way to program this is to use the observer 
    observe_recaptcha_enabled/3.
    
 1. Edit your main site module (/path/to/zotonic/priv/sites/mysite/mysite.erl)
    and add the following lines: 
    
        -export([observe_recaptcha_enabled/3]).
        
        observe_recaptcha_enabled(recaptcha_enabled, Acc, Context) ->
            case Acc of
                true -> 
                    InviationOnly = is_signup_upon_invitation_only(),
                    Path = m_req:get(path, Context),
                    
                    case {InviationOnly, Path} of
                        {true, "/signup"} -> 
                            false; % This disables recaptcha
                            
                        _ ->
                            true
                    end;
                
                _ -> Acc
            end.
            
        is_signup_upon_invitation_only() ->
            true. % You may read this value from a configuration register
            
 2. Recompile Zotonic
 




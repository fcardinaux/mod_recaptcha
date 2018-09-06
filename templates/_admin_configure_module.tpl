<div class="modal-body">

    <div class="form-group">
        <label class="control-label" for="google_recaptcha_site_key">{_ Site key _}</label>
        <div class="controls">
            <input type="text" id="google_recaptcha_site_key" name="google_recaptcha_site_key" value="{{ m.config.mod_recaptcha.site_key.value|escape }}" class="form-control do_autofocus" />
            {% wire id="google_recaptcha_site_key" type="blur" action={config_toggle module="mod_recaptcha" key="site_key" on="keyup"} %}
        </div>
    </div>

    <div class="form-group">
        <label class="control-label" for="google_recaptcha_secret">{_ Secret _}</label>
        <div class="controls">
            <input type="text" id="google_recaptcha_secret" name="google_recaptcha_secret" value="{{ m.config.mod_recaptcha.secret.value|escape }}" class="form-control do_autofocus" />
            {% wire id="google_recaptcha_secret" type="blur" action={config_toggle module="mod_recaptcha" key="secret" on="keyup"} %}
        </div>
    </div>

</div>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} tag="a" %}
</div>


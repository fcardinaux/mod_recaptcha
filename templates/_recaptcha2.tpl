{# See https://developers.google.com/recaptcha/docs/display #}

{% if m.recaptcha.is_enabled %}
    {% with m.recaptcha.site_key as site_key %}
        <script src="https://www.google.com/recaptcha/api.js?&hl={{ m.req.language }}" async defer></script>
        <div class="g-recaptcha" data-sitekey="{{ site_key }}"></div>
    {% endwith %}
{% endif %}

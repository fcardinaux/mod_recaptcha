{#
    Recaptcha template
    Author:
        * for the script: the reCAPTCHA team of Google
        * for the modifications: François Cardinaux, CH 1207 Geneva (http://bit.ly/qTaona)
#}
{% if m.recaptcha.is_enabled %}

{% with m.recaptcha.public_key as public_key %}
<script
    type="text/javascript"
    src="https://www.google.com/recaptcha/api/challenge?k={{ public_key|urlencode }}">
</script>
<noscript>
    <iframe
        src="https://www.google.com/recaptcha/api/noscript?k={{ public_key|urlencode }}"
        height="300" width="500" frameborder="0"></iframe>
    <br>
    <textarea name="recaptcha_challenge_field" rows="3" cols="40"></textarea>
    <input
        type="hidden"
        name="recaptcha_response_field"
        value="manual_challenge">
</noscript>
{% endwith %}

{% endif %}


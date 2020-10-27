---
title: Projects
layout: default
---
<ul class="myposts">
{% for post in site.categories.projects %}
    <li><a href="{{ post.url }}">{{ post.title}}</a>
    <span class="postDate">{{ post.date | date: "%b %-d, %Y" }}</span>
    </li>
{% endfor %}
</ul>

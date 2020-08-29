/*
 * Copyright 2020 The CodeWorld Authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

let fileName = new URLSearchParams(window.location.search).get('path');
if (!fileName) {
  fileName = location.href
    .split('/')
    .pop()
    .replace(/\.html$/, '.json');
}
$.getJSON(fileName, (data) => {
  $.each(data.items, (i, item) => {
    const btn = document.createElement('li');
    const a = document.createElement('a');
    btn.setAttribute('class', 'choice');
    btn.append(a);
    a.innerText = item.name;
    const clickHandler = (event) => {
      $('.choice').removeClass('sel');
      $(btn).addClass('sel');
      $('#screen').attr('src', item.url).focus();
      $('#info-title').text(item.name);
      $('#info-text').text(item.desc || '');
      if (item.code) {
        $('#info-link').show().attr('href', item.code);
      } else {
        $('#info-link').hide();
      }
      event.preventDefault();
    };
    $(a).click(clickHandler);
    // $(btn).click(clickHandler);
    $('#choices').append(btn);
  });
});
$(window).resize((event) => {
  const sz =
    Math.min($('#screen_container').width(), $('#screen_container').height()) -
    50;
  $('#screen').css('width', `${sz}px`);
  $('#screen').css('height', `${sz}px`);
});
$(window).resize();

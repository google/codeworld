import { encode } from './html.js';

async function init() {
  return Promise.resolve(
    $.getScript(
      'https://cdnjs.cloudflare.com/ajax/libs/limonte-sweetalert2/7.19.2/sweetalert2.all.min.js'
    )
  ).catch((e) => console.log('Alert.init failed'));
}

function title(text, iconClass = '') {
  return `<i class="mdi mdi-72px ${iconClass}"></i>&nbsp; ${encode(text)}`;
}

export { init, title };

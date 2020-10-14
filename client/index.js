// What I imagine.
import 'regenerator-runtime';
import * as React from 'react';
import { fetch } from './unison';
import makeHandlers from './handlers';

import { render } from 'react-dom';
import App from './App';

const root = document.createElement('div');
document.body.appendChild(root);

render(<App />, root);

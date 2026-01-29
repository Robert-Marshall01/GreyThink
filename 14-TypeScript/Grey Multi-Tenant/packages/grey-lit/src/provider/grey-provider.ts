/**
 * GreyProvider - Lit Custom Element for context provision
 *
 * Uses @lit-labs/context to provide Grey contexts.
 * Renders a <slot></slot> for child elements.
 * No UI logic.
 */

import { LitElement, html } from 'lit';
import { customElement, property } from 'lit/decorators.js';
import { createContext, provide } from '@lit-labs/context';
import type { GreyConfig } from '@grey/adapters';

// =============================================================================
// Context Definitions
// =============================================================================

export interface GreyContextValue {
  config: GreyConfig;
}

export const greyContext = createContext<GreyContextValue>(Symbol('grey-context'));

// =============================================================================
// GreyProvider Custom Element
// =============================================================================

@customElement('grey-provider')
export class GreyProvider extends LitElement {
  @provide({ context: greyContext })
  @property({ attribute: false })
  value: GreyContextValue = { config: {} };

  render() {
    return html`<slot></slot>`;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'grey-provider': GreyProvider;
  }
}

/**
 * GreyProvider - Stencil Component for context provision
 *
 * Stencil component that provides Grey configuration to child components.
 * Renders a <slot></slot> for child content.
 * No UI logic.
 */

import { Component, Prop, h } from '@stencil/core';
import type { GreyConfig } from '@grey/adapters';

// =============================================================================
// GreyProvider Component
// =============================================================================

@Component({
  tag: 'grey-provider',
  shadow: true,
})
export class GreyProvider {
  /**
   * Grey configuration object
   */
  @Prop() config: GreyConfig = {};

  render() {
    return <slot></slot>;
  }
}

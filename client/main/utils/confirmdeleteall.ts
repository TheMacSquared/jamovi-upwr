import host from '../host';

export async function confirmDeleteAllResults(): Promise<boolean> {
    const message = _('Delete all analyses and results?');
    const warning = _('Warning: deleting all analyses and results cannot be undone.');
    const detail = _('Notes and output columns created by analyses will also be deleted.');

    if (host.isElectron) {
        const response = await host.showMessageBox({
            type: 'warning',
            buttons: [_('Cancel'), _('Delete all')],
            defaultId: 0,
            cancelId: 0,
            noLink: true,
            message,
            detail: `${warning}\n\n${detail}`,
        });
        return response === 1;
    }
    return window.confirm(`${message}\n\n${warning}\n\n${detail}`);
}
